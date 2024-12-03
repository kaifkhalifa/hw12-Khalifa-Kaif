#lang racket


(require rackunit)
(require "hw12.rkt")

;; Define test suite
(define TESTS
  (test-suite
   "HW12 Tests"

   ;; Tests for expr?
   (test-case "Valid expression: bind with addition"
     (check-equal? (expr? '(bind [x 10] (+ x 2))) #t))
   (test-case "Valid expression: function call with multiple arguments"
  (check-equal? (expr? '(+ 1 2 3)) #t))

   ;; Tests for ast?
   (test-case "Valid AST: number"
     (check-equal? (ast? (num 42)) #t))
   (test-case "Invalid AST: plain list"
     (check-equal? (ast? '(42)) #f))

   ;; Tests for error-result?
   (test-case "Valid error result: NOT-FN-ERROR"
     (check-equal? (error-result? 'NOT-FN-ERROR) #t))
   (test-case "Invalid error result: number"
     (check-equal? (error-result? 42) #f))

   ;; Tests for NaN?
   (test-case "Valid NaN Result"
     (check-equal? (NaN? 'NaN) #t))
   (test-case "Invalid NaN Result: number"
     (check-equal? (NaN? 42) #f))

   ;; Tests for UNDEFINED-ERROR?
   (test-case "Valid undefined error"
     (check-equal? (UNDEFINED-ERROR? 'UNDEFINED-ERROR) #t))
   (test-case "Invalid undefined error: symbol"
     (check-equal? (UNDEFINED-ERROR? 'OTHER-ERROR) #f))

   ;; Tests for NOT-FN-ERROR?
   (test-case "Valid not-function error"
     (check-equal? (NOT-FN-ERROR? 'NOT-FN-ERROR) #t))
   (test-case "Invalid not-function error: other error"
     (check-equal? (NOT-FN-ERROR? 'ARITY-ERROR) #f))

   ;; Tests for ARITY-ERROR?
   (test-case "Valid arity error"
     (check-equal? (ARITY-ERROR? 'ARITY-ERROR) #t))
   (test-case "Invalid arity error: number"
     (check-equal? (ARITY-ERROR? 42) #f))

   ;; Tests for parse
   (test-case "Parse valid number"
     (check-equal? (parse 42) (num 42)))
   (test-case "Parse valid bind expression"
     (check-equal? (parse '(bind [x 10] (+ x 2)))
                   (bind 'x (num 10) (call (vari '+) (list (vari 'x) (num 2))))))
   (test-case "Parse nested bind expression"
  (check-equal? (parse '(bind [x 10] (bind [y 20] (+ x y))))
                (bind 'x (num 10)
                      (bind 'y (num 20)
                            (call (vari '+) (list (vari 'x) (vari 'y)))))))

   ;; Tests for env-add
   (test-case "Add new binding to empty environment"
     (check-equal? (env-add '() 'x 10) '((x 10))))
   (test-case "Add new binding to existing environment"
     (check-equal? (env-add '((y 20)) 'x 10) '((x 10) (y 20))))

   ;; Tests for run
   (test-case "Run a simple number"
     (check-equal? (run (num 42)) 42))
   (test-case "Run a variable"
  (check-equal? ((run (vari '+)) 1 2 3) 6))
   (test-case "Run an addition"
     (check-equal? (run (call (vari '+) (list (num 5) (num 10)))) 15))
   (test-case "Run an undefined variable"
     (check-equal? (run (vari 'z)) 'UNDEFINED-ERROR))
   (test-case "Run a function call with one argument instead of two"
  (check-equal? (run (call (fn-ast '(x) (call (vari '+) (list (vari 'x) (num 10))))
                        (list (num 5))))
                15))
   (test-case "Run a function applied to NaN"
     (check-equal? (run (call (vari '+) (list (num 5) 'NaN))) 'NaN))
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)