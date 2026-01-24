(load "constant-folding.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-fold name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Folded: ") (pretty-print (constant-folding expr))
  (newline))

;; Arithmetic operations
(test-fold "Addition"
  '(+ 2 3))

(test-fold "Subtraction"
  '(- 10 4))

(test-fold "Multiplication"
  '(* 5 6))

(test-fold "Division"
  '(/ 20 4))

(test-fold "Nested Arithmetic"
  '(+ (* 2 3) (- 10 5)))

(test-fold "Mixed with Variables"
  '(+ 2 x 3))

;; Comparison operations
(test-fold "Less Than True"
  '(< 3 5))

(test-fold "Less Than False"
  '(< 5 3))

(test-fold "Equal True"
  '(= 5 5))

(test-fold "Equal False"
  '(= 5 6))

;; Boolean operations
(test-fold "Not True"
  '(not #f))

(test-fold "Not False"
  '(not #t))

(test-fold "And All True"
  '(and #t #t #t))

(test-fold "And Some False"
  '(and #t #f #t))

(test-fold "Or All False"
  '(or #f #f #f))

(test-fold "Or Some True"
  '(or #f #t #f))

;; If with constant condition
(test-fold "If True"
  '(if #t 1 2))

(test-fold "If False"
  '(if #f 1 2))

(test-fold "If with Folded Condition"
  '(if (< 3 5) (+ 1 2) (+ 3 4)))

;; Complex nested example
(test-fold "Complex Nested"
  '(if (and #t (< 2 5))
       (+ (* 3 4) 5)
       (- 10 2)))

;; Let with constant values
(test-fold "Let with Constants"
  '(let ((x 5)
         (y 10))
     (+ x y)))

;; Mixed - some foldable, some not
(test-fold "Partial Folding"
  '(lambda (x)
     (+ (* 2 3) x)))

;; Begin flattening and folding
(test-fold "Begin with Constants"
  '(begin 
     (+ 1 2)
     (+ 3 4)))

;; Define with constant
(test-fold "Define with Constant"
  '(define pi (* 3 1.0)))

;; No folding - variables only
(test-fold "Variables Only"
  '(+ x y z))

;; Division by zero - should not fold
(test-fold "Division by Zero"
  '(/ 10 0))
