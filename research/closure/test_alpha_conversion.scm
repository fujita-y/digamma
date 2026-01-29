;; test_alpha_conversion.scm
;; Test suite for alpha conversion (variable renaming).

(load "alpha_conversion.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (alpha-conversion expr)))
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
;; Section 1: Basic Alpha Conversion
;; =============================================================================
(display "\n>>> Section 1: Basic Alpha Conversion\n")

(test "Identity"
      '(lambda (x) x)
      '(lambda (x.1) x.1))

(test "Shadowing"
      '(lambda (x) (lambda (x) x))
      '(lambda (x.1) (lambda (x.2) x.2)))

(test "Let Shadowing"
      '(let ((x 1)) (let ((x 2)) x))
      '(let ((x.1 1)) (let ((x.2 2)) x.2)))

;; =============================================================================
;; Section 2: Definitions and Free Variables
;; =============================================================================
(display "\n>>> Section 2: Definitions and Free Variables\n")

(test "Define Function 1"
      '(define (add x y) (+ x y))
      '(define (add x.1 y.2) (+ x.1 y.2)))

(test "Define Function 2"
      '(define add (lambda (x y) (+ x y)))
      '(define add (lambda (x.1 y.2) (+ x.1 y.2))))

(test "Free Variables"
      '(lambda (x) (+ x y))
      '(lambda (x.1) (+ x.1 y)))

;; =============================================================================
;; Section 3: Complex Nested Scopes
;; =============================================================================
(display "\n>>> Section 3: Complex Nested Scopes\n")

(test "Complex Nested"
      '(define (foo a)
         (let ((b (+ a 1)))
           (lambda (c)
             (let ((a (* c 2)))
               (+ a b)))))
      '(define (foo a.1)
         (let ((b.2 (+ a.1 1)))
           (lambda (c.3)
             (let ((a.4 (* c.3 2)))
               (+ a.4 b.2))))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
