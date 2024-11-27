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
(check-equal? (error-result? 'NOT-FN-ERROR) #t)


;; NaN? : Any -> Boolean
;; Returns true if given a NaN Result.
(define (NaN? x)
  (eq? x 'NaN))
(check-equal? (NaN? 'NaN) #t)


;; UNDEFINED-ERROR? : Any -> Boolean
;; Returns true if given a UNDEFINED-ERROR Result.
(define (UNDEFINED-ERROR? x)
  (eq? x 'UNDEFINED-ERROR))
(check-equal? (UNDEFINED-ERROR? 'UNDEFINED-ERROR) #t)

;; NOT-FN-ERROR? : Any -> Boolean
;; Returns true if given a NOT-FN-ERROR Result.
(define (NOT-FN-ERROR? x)
  (eq? x 'NOT-FN-ERROR))
(check-equal? (NOT-FN-ERROR? 'NOT-FN-ERROR) #t)

;; ARITY-ERROR? : Any -> Boolean
;; Returns true if given a ARITY-ERROR Result.
(define (ARITY-ERROR? x)
  (eq? x 'ARITY-ERROR))
(check-equal? (ARITY-ERROR? 'ARITY-ERROR) #t)

;; FUNCTIONS

;; parse: Expr -> AST
;; Converts a 450LangExpr into a 450LangAST.
;; Raises exn:fail:syntax:cs450 if the input is not a valid Expr.
(define (parse expr)
  (cond
    ;; Case 1: Numbers
    [(number? expr) (num expr)]
    
    ;; Case 2: Variables
    [(symbol? expr) (vari expr)]
    
    ;; Case 3: Bind expressions
    [(and (list? expr)
          (eq? (first expr) 'bind)
          (= (length expr) 3) ;; Ensure there are exactly three elements
          (list? (second expr)) ;; Ensure second element is a list
          (= (length (second expr)) 2) ;; Ensure it has two elements
          (var? (first (second expr))) ;; Ensure first element is a variable
          (expr? (second (second expr))) ;; Ensure second element is a valid expr
          (expr? (third expr))) ;; Ensure third element is a valid expr
     (bind (first (second expr))
           (parse (second (second expr)))
           (parse (third expr)))]

    ;; Case 4: Function expressions
    [(and (list? expr)
          (eq? (first expr) 'fn)
          (list? (second expr)) ;; Ensure second element is a list
          (andmap var? (second expr))) ;; All elements in the list are variables
     (if (< (length expr) 3) ;; Ensure a body exists
         (raise (exn:fail:syntax:cs450 "Invalid function syntax: Missing body"))
         (fn-ast (second expr) (parse (third expr))))]

    ;; Case 5: Function calls
    [(and (pair? expr)
          (expr? (car expr)) ;; Function must be a valid expression
          (andmap expr? (cdr expr))) ;; All arguments must be valid expressions
     (call (parse (car expr)) (map parse (cdr expr)))]

    ;; Case 6: Invalid expressions
    [else (raise (exn:fail:syntax:cs450 "Invalid CS450LangExpr"))]))


;; env-add: Env Var Result -> Env
;; Adds a new binding to the environment.
(define (env-add env var result)
  (if (list? env)
      (cons (list var result) env)
      (raise (error "env-add: Invalid environment (not a list)"))))
(check-equal? (env-add '((y 20)) 'x 10) '((x 10) (y 20)))

;; init-env: -> Environment
;; Initializes the environment with predefined bindings for `+` and `-`.
;; Produces a list of initial variable bindings.
(define (init-env)
  (list
   (list '+ (lambda args (if (andmap number? args) (apply + args) 'NaN)))
   (list '- (lambda args (if (andmap number? args) (apply - args) 'NaN)))))
(check-equal? (map first (init-env)) '(+ -))

;; 450apply: Result (Listof Result) -> Result
;; Applies a function `fn` to the arguments `args` in the given environment.
;; Produces the result of the function application or an error result if the application is invalid.
(define (450apply fn args)
  (match fn
    [(? procedure?) (if (andmap number? args) (apply fn args) 'NaN)]
    [(fn-result params body env)
     (if (= (length params) (length args))
         (run/env body (foldl env-add env params args))
         'ARITY-ERROR)]
    [_ 'NOT-FN-ERROR]))
(check-equal? (450apply (lambda args (apply + args)) (list 1 2 3)) 6)

;; run/env: AST Environment -> Result
;; Evaluates a `450LangAST` tree within a given environment `env`.
;; Produces the result of evaluation or an error result for invalid operations.
(define (run/env ast env)
  (match ast
    ;; Case 1: Number literals
    [(num n) n]
    
    ;; Case 2: Variables
    [(vari v)
     (let ([binding (assoc v env)]) ;; Find the variable in the environment
       (if binding
           (second binding) ;; Return its value if found
           'UNDEFINED-ERROR))] ;; Otherwise, return an error
    
    ;; Case 3: Function definitions
    [(fn-ast params body)
     (fn-result params body env)] ;; Attach the current environment to the function
    
    ;; Case 4: Function calls
    [(call fn args)
     (450apply (run/env fn env) (map (curryr run/env env) args))] ;; Evaluate function and arguments
    
    ;; Case 5: Bind expressions
    [(bind var expr body)
     (let ([val (run/env expr env)]) ;; Evaluate the expression
       (if (result? val) ;; Ensure it evaluates to a valid result
           (run/env body (env-add env var val)) ;; Add to environment and continue
           'UNDEFINED-ERROR))] ;; Return an error if `expr` is invalid
    
    ;; Case 6: Invalid AST
    [_ 'UNDEFINED-ERROR]))

(check-equal? (run/env (num 42) '()) 42)

;; run: AST -> Result
;; Evaluates a `450LangAST` tree in the initial environment.
;; Produces the result of evaluation or an error result for invalid operations.
(define (run ast)
  (run/env ast (init-env)))
(check-equal? (run (num 42)) 42)


