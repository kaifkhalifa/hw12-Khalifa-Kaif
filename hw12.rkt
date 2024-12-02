#lang racket

(provide (all-defined-out)) 

(require rackunit)


;; DATA DEFINITIONS

;; A 450LangResult (Result) is one of:
;; - Number
;; - 'NaN
;; - FnResult
;; - ErrorResult
(define (result? x)
  (or (number? x)
      (eq? x 'NaN)
      (fn-result? x)
      (error-result? x)))

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
;; example
(check-equal? (expr? '(bind [x 10] (+ x 2))) #t)


;; A 450LangAST (AST) is one of:
;; - (num Number)
;; - (vari Symbol)
;; - (fn-ast List<Symbol> AST)
;; - (call AST List<AST>)
;; - (bind Symbol AST AST)
(struct num [n] #:transparent)
(struct vari [v] #:transparent)
(struct fn-ast [params body] #:transparent)
(struct call [fn args] #:transparent)
(struct bind [var expr body] #:transparent)

;; A FnResult is one of:
;; - A Racket function
;; - (fn-result List<Symbol> AST Environment)
(struct fn-result [params body env] #:transparent)

(define (ast? x)
  (or (num? x)
      (vari? x)
      (fn-ast? x)
      (call? x)
      (bind? x)))
;; example
(check-equal? (ast? (num 42)) #t)


;; exn:fail:syntax:cs450 is a subtype of exn:fail:syntax for CS450 syntax errors.
(struct exn:fail:syntax:cs450 exn:fail:syntax (message))

;; An ErrorResult is one of:
;; - 'UNDEFINED-ERROR
;; - 'NOT-FN-ERROR
;; - 'ARITY-ERROR
(define (error-result? x)
  (or (eq? x 'UNDEFINED-ERROR)
      (eq? x 'NOT-FN-ERROR)
      (eq? x 'ARITY-ERROR)))
;; example
(check-equal? (error-result? 'NOT-FN-ERROR) #t)


;; NaN? : Any -> Boolean
;; Returns true if given a NaN Result.
(define (NaN? x)
  (eq? x 'NaN))
;; example
(check-equal? (NaN? 'NaN) #t)


;; UNDEFINED-ERROR? : Any -> Boolean
;; Returns true if given a UNDEFINED-ERROR Result.
(define (UNDEFINED-ERROR? x)
  (eq? x 'UNDEFINED-ERROR))
;; example
(check-equal? (UNDEFINED-ERROR? 'UNDEFINED-ERROR) #t)

;; NOT-FN-ERROR? : Any -> Boolean
;; Returns true if given a NOT-FN-ERROR Result.
(define (NOT-FN-ERROR? x)
  (eq? x 'NOT-FN-ERROR))
;; example
(check-equal? (NOT-FN-ERROR? 'NOT-FN-ERROR) #t)

;; ARITY-ERROR? : Any -> Boolean
;; Returns true if given a ARITY-ERROR Result.
(define (ARITY-ERROR? x)
  (eq? x 'ARITY-ERROR))
;; example
(check-equal? (ARITY-ERROR? 'ARITY-ERROR) #t)

;; FUNCTIONS

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
     (bind (first (second expr))
           (parse (second (second expr)))
           (parse (third expr)))]
    [(and (list? expr) (eq? (first expr) 'fn) (list? (second expr))
          (andmap symbol? (second expr)) (expr? (third expr)))
     (fn-ast (second expr) (parse (third expr)))]
    [(and (pair? expr) (expr? (car expr)) (andmap expr? (cdr expr)))
     (call (parse (car expr)) (map parse (cdr expr)))]
    [else (raise (exn:fail:syntax:cs450 "Invalid CS450LangExpr"))]))
;; example
  (check-equal? (parse '(bind [x 10] (bind [y 20] (+ x y))))
                (bind 'x (num 10)
                      (bind 'y (num 20)
                            (call (vari '+) (list (vari 'x) (vari 'y))))))


;; env-add: Env Var Result -> Env
;; Adds a new binding to the environment.
(define (env-add env var result)
  (cons (list var result) env))
;; example
(check-equal? (env-add '((y 20)) 'x 10) '((x 10) (y 20)))

;; init-env: -> Environment
;; Initializes the environment with predefined bindings for `+` and `-`.
;; Produces a list of initial variable bindings.
(define (init-env)
  (list
   (list '+ (lambda args (if (andmap number? args) (apply + args) 'NaN)))
   (list '- (lambda args (if (andmap number? args) (apply - args) 'NaN)))))
;; example
(check-equal? (map first (init-env)) '(+ -))

;; 450apply: Result (Listof Result) -> Result
;; Applies a function `fn` to the arguments `args` in the given environment.
;; Produces the result of the function application or an error result if the application is invalid.
(define (450apply fn args)
  (match fn
    [(? procedure?) (if (andmap number? args) (apply fn args) 'NaN)]
    [(fn-result params body env)
     (unless (list? env)
       (raise (error (format "450apply: Invalid environment ~a (not a list)" env))))
     (if (= (length params) (length args))
         (let ([new-env (foldl env-add env params args)])
           (run/env body new-env))
         'ARITY-ERROR)]
    [_ 'NOT-FN-ERROR]))
;; example
(check-equal? (450apply (lambda args (apply + args)) (list 1 2 3)) 6)

;; run/env: AST Environment -> Result
;; Evaluates a `450LangAST` tree within a given environment `env`.
;; Produces the result of evaluation or an error result for invalid operations.
(define (run/env ast env)
  (match ast
    [(num n) n]
    [(vari v)
     (let ([binding (assoc v env)])
       (if binding (second binding) 'UNDEFINED-ERROR))]
    [(fn-ast params body) (fn-result params body env)]
    [(call fn args)
     (450apply (run/env fn env) (map (curryr run/env env) args))]
    [(bind var expr body)
     (let ([val (run/env expr env)])
       (if (result? val)
           (run/env body (env-add env var val))
           'UNDEFINED-ERROR))]
    [_ 'UNDEFINED-ERROR]))
;; example
(check-equal? (run/env (num 42) '()) 42)

;; run: AST -> Result
;; Evaluates a `450LangAST` tree in the initial environment.
;; Produces the result of evaluation or an error result for invalid operations.
(define (run ast)
  (run/env ast (init-env)))
;; example
(check-equal? (run (num 42)) 42)