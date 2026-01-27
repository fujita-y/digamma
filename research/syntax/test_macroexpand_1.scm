;; test_macroexpand_1.scm
;; Basic macro expansion and identifier macro tests.

(load "./macroexpand.scm")

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; =============================================================================
;; Section 1: Simple macro
;; =============================================================================
(display "\n>>> Section 1: Simple macro\n")
(expand '(define-syntax foo (syntax-rules ()
                               ((foo x) (list 'foo x)))))

(define test1 (macroexpand-1 '(foo 1)))
(display "Test 1 Expand: ") (write test1) (newline)

;; =============================================================================
;; Section 2: Nested macro
;; =============================================================================
(display "\n>>> Section 2: Nested macro\n")
(expand '(define-syntax bar (syntax-rules ()
                               ((bar x) (foo x)))))

(define test2 (macroexpand-1 '(bar 2)))
(display "Test 2 Expand: ") (write test2) (newline)

(define test2b (macroexpand-1 test2))
(display "Test 2b Expand: ") (write test2b) (newline)

;; =============================================================================
;; Section 3: Non macro and Identifier macro
;; =============================================================================
(display "\n>>> Section 3: Non macro and Identifier macro\n")
(test "Non-macro form" (macroexpand-1 '(list 1 2)) '(list 1 2))

(register-macro! 'baz (lambda (expr) 'expanded-baz))
(test "Identifier macro" (macroexpand-1 'baz) 'expanded-baz)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
