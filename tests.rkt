#lang racket

;; tests.rkt
;; Test suite for hw12.rkt using rackunit.

(require rackunit)
(require "hw12.rkt") ; Make sure this points to your implementation file.

;; Define test suite
(define TESTS
  (test-suite
   "HW12 Tests"

   ;; Tests for expr?
   (test-case "Valid expression: bind with addition"
     (check-equal? (expr? '(bind [x 10] (+ x 2))) #t))
   (test-case "Invalid expression: missing brackets"
     (check-equal? (expr? '(bind x 10 (+ x 2))) #f))

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
   (test-case "Invalid parse: unknown expression"
     (check-exn exn:fail:syntax:cs450?
       (λ () (parse '(invalid 42)))))

   ;; Tests for env-add
   (test-case "Add new binding to empty environment"
     (check-equal? (env-add '() 'x 10) '((x 10))))
   (test-case "Add new binding to existing environment"
     (check-equal? (env-add '((y 20)) 'x 10) '((x 10) (y 20))))

   ;; Tests for run
   (test-case "Run a simple number"
     (check-equal? (run (num 42)) 42))
   (test-case "Run a variable"
     (check-equal? (run (vari '+)) (lambda args (if (andmap number? args) (apply + args) 'NaN))))
   (test-case "Run an addition"
     (check-equal? (run (call (vari '+) (list (num 5) (num 10)))) 15))
   (test-case "Run a bind with a function"
     (check-equal?
      (run (bind 'x (num 10) (call (fn-ast '(y) (call (vari '+) (list (vari 'x) (vari 'y)))) (list (num 20)))))
      30))
   (test-case "Run an undefined variable"
     (check-equal? (run (vari 'z)) 'UNDEFINED-ERROR))
   (test-case "Run a function call with wrong arity"
     (check-equal? (run (call (fn-ast '(x y) (num 42)) (list (num 5)))) 'ARITY-ERROR))
   (test-case "Run a function applied to NaN"
     (check-equal? (run (call (vari '+) (list (num 5) 'NaN))) 'NaN))
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)