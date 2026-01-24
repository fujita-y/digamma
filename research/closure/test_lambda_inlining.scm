(load "lambda_inlining.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-optimize name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Optimized: ") (pretty-print (lambda-inlining expr))
  (newline))

;; Test 1: Simple lambda application
(test-optimize "Simple Lambda Application"
  '((lambda (x) (+ x 1)) 5))

;; Test 2: Multiple arguments
(test-optimize "Multiple Arguments"
  '((lambda (x y) (+ x y)) 3 4))

;; Test 3: Let expression (should convert to lambda and inline)
(test-optimize "Let Expression"
  '(let ((x 10)) (+ x 5)))

;; Test 4: Nested let
(test-optimize "Nested Let"
  '(let ((x 5))
     (let ((y 10))
       (+ x y))))

;; Test 5: Lambda with variable reference (used once - should inline)
(test-optimize "Lambda Used Once"
  '((lambda (x) (+ x x)) (f y)))
  ;; x is used twice, so won't inline if arg is complex

;; Test 6: Lambda with simple arg (should always inline)
(test-optimize "Lambda Simple Arg"
  '((lambda (x) (* x x)) 5))

;; Test 7: Shadowing
(test-optimize "Shadowing"
  '((lambda (x) 
      (let ((x 10)) 
        x)) 
    5))

;; Test 8: No inlining - arity mismatch
(test-optimize "Arity Mismatch"
  '((lambda (x y) (+ x y)) 5))

;; Test 9: Complex - let with body using variable multiple times
(test-optimize "Let Multiple Uses"
  '(let ((x (expensive-computation)))
     (+ x x x)))
  ;; Won't inline if used > 1 time

;; Test 10: Simple let with multiple uses
(test-optimize "Simple Let Multiple Uses"
  '(let ((x 42))
     (+ x x)))
  ;; Will inline because 42 is simple

;; Test 11: Nested lambda applications
(test-optimize "Nested Lambda Apps"
  '((lambda (x) 
      ((lambda (y) (+ x y)) 10)) 
    5))

;; Test 12: Begin flattening
(test-optimize "Begin Flattening"
  '(begin (begin 1 2) 3))

;; Test 13: Define with lambda
(test-optimize "Define with Lambda Body"
  '(define (foo x)
     (let ((y 10))
       (+ x y))))

;; Test 14: Lambda returning lambda (no immediate application)
(test-optimize "Higher-order No Inline"
  '(lambda (x) (lambda (y) (+ x y))))
