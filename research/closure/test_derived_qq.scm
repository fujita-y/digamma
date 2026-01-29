;; test_derived_qq.scm
;; Test suite for quasiquote expansion through derived_syntax transformation.

(load "derived_syntax.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (expand-derived-syntax expr)))
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
;; Section 1: Quasiquote Expansion
;; =============================================================================
(display "\n>>> Section 1: Quasiquote Expansion\n")

(test "Quasiquote Simple"
      '`(a b c)
      '(list 'a 'b 'c))

(test "Quasiquote with Unquote"
      '`(a ,x b)
      '(list 'a x 'b))

(test "Quasiquote with Unquote-Splicing"
      '`(a ,@x b)
      '(cons 'a (append x (cons 'b ()))))

(test "Quasiquote in Lambda"
      '(lambda (x) `(result ,x))
      '(lambda (x) (list 'result x)))

(test "Combined: Let* and Quasiquote"
      '(let* ((x 1) (y 2)) `(values ,x ,y))
      '(let ((x 1)) (let ((y 2)) (list 'values x y))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
