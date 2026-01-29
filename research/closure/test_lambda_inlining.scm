;; test_lambda_inlining.scm
;; Test suite for lambda inlining optimization.

(load "lambda_inlining.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (lambda-inlining expr)))
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
;; Section 1: Basic Inlining
;; =============================================================================
(display "\n>>> Section 1: Basic Inlining\n")

(test "Simple Lambda Application"
      '((lambda (x) (+ x 1)) 5)
      '(+ 5 1))

(test "Multiple Arguments"
      '((lambda (x y) (+ x y)) 3 4)
      '(+ 3 4))

(test "Let Expression"
      '(let ((x 10)) (+ x 5))
      '(+ 10 5))

(test "Nested Let"
      '(let ((x 5)) (let ((y 10)) (+ x y)))
      '(+ 5 10))

;; =============================================================================
;; Section 2: Complexity and Thresholds
;; =============================================================================
(display "\n>>> Section 2: Complexity and Thresholds\n")

(test "Lambda Used Once"
      '((lambda (x) (+ x x)) (f y))
      '((lambda (x) (+ x x)) (f y)))

(test "Lambda Simple Arg"
      '((lambda (x) (* x x)) 5)
      '(* 5 5))

(test "Let Multiple Uses"
      '(let ((x (expensive-computation))) (+ x x x))
      '((lambda (x) (+ x x x)) (expensive-computation)))

(test "Simple Let Multiple Uses"
      '(let ((x 42)) (+ x x))
      '(+ 42 42))

;; =============================================================================
;; Section 3: Shadowing and Arity
;; =============================================================================
(display "\n>>> Section 3: Shadowing and Arity\n")

(test "Shadowing"
      '((lambda (x) (let ((x 10)) x)) 5)
      10)

(test "Arity Mismatch"
      '((lambda (x y) (+ x y)) 5)
      '((lambda (x y) (+ x y)) 5))

;; =============================================================================
;; Section 4: Integration with Other Forms
;; =============================================================================
(display "\n>>> Section 4: Integration with Other Forms\n")

(test "Nested Lambda Apps"
      '((lambda (x) ((lambda (y) (+ x y)) 10)) 5)
      '(+ 5 10))

(test "Begin Flattening"
      '(begin (begin 1 2) 3)
      '(begin 1 2 3))

(test "Define with Lambda Body"
      '(define (foo x) (let ((y 10)) (+ x y)))
      '(define (foo x) (+ x 10)))

(test "Higher-order No Inline"
      '(lambda (x) (lambda (y) (+ x y)))
      '(lambda (x) (lambda (y) (+ x y))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
