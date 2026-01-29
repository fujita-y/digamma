;; test_constant_folding.scm
;; Test suite for constant folding optimization.

(load "constant_folding.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (constant-folding expr)))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

;; =============================================================================
;; Section 1: Arithmetic Operations
;; =============================================================================
(display "\n>>> Section 1: Arithmetic Operations\n")

(test "Addition" '(+ 2 3) 5)
(test "Subtraction" '(- 10 4) 6)
(test "Multiplication" '(* 5 6) 30)
(test "Division" '(/ 20 4) 5)
(test "Nested Arithmetic" '(+ (* 2 3) (- 10 5)) 11)
(test "Mixed with Variables" '(+ 2 x 3) '(+ 2 x 3))

;; =============================================================================
;; Section 2: Comparison Operations
;; =============================================================================
(display "\n>>> Section 2: Comparison Operations\n")

(test "Less Than True" '(< 3 5) #t)
(test "Less Than False" '(< 5 3) #f)
(test "Equal True" '(= 5 5) #t)
(test "Equal False" '(= 5 6) #f)

;; =============================================================================
;; Section 3: Boolean Operations
;; =============================================================================
(display "\n>>> Section 3: Boolean Operations\n")

(test "Not True" '(not #f) #t)
(test "Not False" '(not #t) #f)
(test "And All True" '(and #t #t #t) #t)
(test "And Some False" '(and #t #f #t) #f)
(test "Or All False" '(or #f #f #f) #f)
(test "Or Some True" '(or #f #t #f) #t)

;; =============================================================================
;; Section 4: Control Flow
;; =============================================================================
(display "\n>>> Section 4: Control Flow\n")

(test "If True" '(if #t 1 2) 1)
(test "If False" '(if #f 1 2) 2)
(test "If with Folded Condition" '(if (< 3 5) (+ 1 2) (+ 3 4)) 3)
(test "Complex Nested" '(if (and #t (< 2 5)) (+ (* 3 4) 5) (- 10 2)) 17)

;; =============================================================================
;; Section 5: Other Forms
;; =============================================================================
(display "\n>>> Section 5: Other Forms\n")

(test "Let with Constants" '(let ((x 5) (y 10)) (+ x y)) '(let ((x 5) (y 10)) (+ x y)))
(test "Partial Folding" '(lambda (x) (+ (* 2 3) x)) '(lambda (x) (+ 6 x)))
(test "Begin with Constants" '(begin (+ 1 2) (+ 3 4)) '(begin 3 7))
(test "Define with Constant" '(define pi (* 3 1.0)) '(define pi 3.0))
(test "Variables Only" '(+ x y z) '(+ x y z))
(test "Division by Zero" '(/ 10 0) '(/ 10 0))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
