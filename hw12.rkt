#lang racket

(provide (all-defined-out)) 

(require rackunit)

;; ============================
;; DATA DEFINITIONS
;; ============================

;; A Var is a Symbol.
;; Interpretation: Represents a variable in the CS450Lang program.
(define (var? x) (symbol? x))

;; An Environment is a List of (list Var Result).
;; Interpretation: Maps variables to their results in a program. Earlier bindings shadow later ones.
(define (env? x)
  (and (list? x)
       (andmap (lambda (entry)
                 (and (list? entry)
                      (= (length entry) 2)
                      (var? (first entry))
                      (result? (second entry))))
               x)))

;; A 450LangExpr (Expr) is one of:
;; - Number
;; - Var
;; - '(bind [Var Expr] Expr)
;; - '(fn List<Var> Expr)
;; - (cons Expr List<Expr>)
;; Interpretation: Represents the syntax of CS450Lang programs.
(define (expr? x)
  (or (number? x)
      (var? x)
      (and (list? x)
           (or (and (eq? (first x) 'bind)
                    (list? (second x))
                    (= (length (second x)) 2)
                    (var? (first (second x)))
                    (expr? (second (second x)))
                    (expr? (third x)))
               (and (eq? (first x) 'fn)
                    (list? (second x))
                    (andmap var? (second x))
                    (expr? (third x)))
               (and (pair? x)
                    (expr? (car x))
                    (andmap expr? (cdr x)))))))

;; A 450LangAST (AST) is one of:
;; - (num Number)
;; - (vari Symbol)
;; - (fn-ast List<Symbol> AST)
;; - (call AST List<AST>)
;; Interpretation: Represents the parsed version of CS450Lang programs.
(struct num [n])
(struct vari [v])
(struct fn-ast [params body])
(struct call [fn args])

(define (ast? x)
  (or (num? x)
      (vari? x)
      (fn-ast? x)
      (call? x)))

;; A 450LangResult (Result) is one of:
;; - Number
;; - 'NaN
;; - FnResult
;; - ErrorResult
;; Interpretation: Represents the result of running a CS450Lang program.
(define (result? x)
  (or (number? x)
      (eq? x 'NaN)
      (fn-result? x)
      (error-result? x)))

;; A FnResult is one of:
;; - A Racket function
;; - (fn-result List<Symbol> AST Environment)
(struct fn-result [params body env])

(define (fn-result? x)
  (or (procedure? x)
      (and (struct? x 'fn-result)
           (list? (fn-result-params x))
           (andmap symbol? (fn-result-params x))
           (ast? (fn-result-body x))
           (env? (fn-result-env x)))))

;; An ErrorResult is one of:
;; - 'UNDEFINED-ERROR
;; - 'NOT-FN-ERROR
;; - 'ARITY-ERROR
(define (error-result? x)
  (or (eq? x 'UNDEFINED-ERROR)
      (eq? x 'NOT-FN-ERROR)
      (eq? x 'ARITY-ERROR)))

;; ============================
;; FUNCTIONS
;; ============================

;; exn:fail:syntax:cs450 is a subtype of exn:fail:syntax for CS450 syntax errors.
(struct exn:fail:syntax:cs450 exn:fail:syntax ())

;; exn:fail:syntax:cs450? : Any -> Boolean
;; Returns true if given an exn:fail:syntax:cs450 exception.
(define (exn:fail:syntax:cs450? x)
  (and (exn:fail:syntax? x)
       (struct? x 'exn:fail:syntax:cs450)))

;; NaN? : Any -> Boolean
;; Returns true if given a NaN Result.
(define (NaN? x)
  (eq? x 'NaN))

;; UNDEFINED-ERROR? : Any -> Boolean
;; Returns true if given an UNDEFINED-ERROR Result.
(define (UNDEFINED-ERROR? x)
  (eq? x 'UNDEFINED-ERROR))

;; NOT-FN-ERROR? : Any -> Boolean
;; Returns true if given a NOT-FN-ERROR Result.
(define (NOT-FN-ERROR? x)
  (eq? x 'NOT-FN-ERROR))

;; ARITY-ERROR? : Any -> Boolean
;; Returns true if given an ARITY-ERROR Result.
(define (ARITY-ERROR? x)
  (eq? x 'ARITY-ERROR))

;; parse: Expr -> AST
;; Converts a 450LangExpr into a 450LangAST.
;; Raises exn:fail:syntax:cs450 if the input is not a valid Expr.
(define (parse expr)
  (cond
    [(number? expr) (num expr)]
    [(symbol? expr) (vari expr)]
    [(and (list? expr) (eq? (first expr) 'bind) (= (length expr) 3)
          (list? (second expr)) (= (length (second expr)) 2)
          (symbol? (first (second expr))) (expr? (second (second expr)))
          (expr? (third expr)))
     (bind (first (second expr)) (parse (second (second expr))) (parse (third expr)))]
    [(and (list? expr) (eq? (first expr) 'fn) (list? (second expr))
          (andmap symbol? (second expr)) (expr? (third expr)))
     (fn-ast (second expr) (parse (third expr)))]
    [(and (pair? expr) (expr? (car expr)) (andmap expr? (cdr expr)))
     (call (parse (car expr)) (map parse (cdr expr)))]
    [else (raise (exn:fail:syntax:cs450 "Invalid CS450LangExpr" (current-continuation-marks)))]))

;; run: AST -> Result
;; Evaluates a 450LangAST tree to produce a 450LangResult.
(define (run ast)
  (define INIT-ENV
    (list (list '+ (lambda args (if (andmap number? args) (apply + args) 'NaN)))
          (list '- (lambda args (if (andmap number? args) (apply - args) 'NaN)))))
  (define (450apply fn args)
    (match fn
      [(? procedure?) (if (andmap number? args) (apply fn args) 'NaN)]
      [(fn-result params body env)
       (if (= (length params) (length args))
           (run/env body (foldl env-add env params args))
           'ARITY-ERROR)]
      [_ 'NOT-FN-ERROR]))
  (define (run/env ast env)
    (match ast
      [(num n) n]
      [(vari v) (match (assoc v env) [#f 'UNDEFINED-ERROR] [(_ . result) result])]
      [(fn-ast params body) (fn-result params body env)]
      [(call fn args) (450apply (run/env fn env) (map (curryr run/env env) args))]
      [_ 'UNDEFINED-ERROR]))
  (run/env ast INIT-ENV))
