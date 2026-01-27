;; test_r7rs_extra.scm
;; Test suite for extra R7RS features in syntax-rules.

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
;; Section 1: R7RS syntax-rules Features
;; =============================================================================
(display "\n>>> Section 1: R7RS syntax-rules Features\n")

(macroexpand
 '(define-syntax nested-rules
    (syntax-rules ()
      ((_ (x ...))
       (let-syntax ((inner (syntax-rules dots ()
                             ((_ a dots) (list x ... a dots)))))
         (inner 1 2 3))))))

(test "nested-rules-ellipses" (macroexpand '(nested-rules (a b)) 'strip) '(list a b 1 2 3))

(macroexpand
 '(define-syntax lit-shadow
    (syntax-rules (=>)
      ((_ a => b) (list 'match a b))
      ((_ a b c) (list 'no-match a b c)))))

(test "lit-shadow-match" (macroexpand '(lit-shadow 1 => 2) 'strip) '(list 'match 1 2))

(test "lit-shadow-no-match"
      (macroexpand '(let ((=> #f)) (lit-shadow 1 => 2)) 'strip)
      '(let ((=> #f)) (list 'no-match 1 => 2)))

(macroexpand
 '(define-syntax improper-elli
    (syntax-rules ()
      ((_ (a ... . b)) (list (list a ...) b)))))

(test "improper-elli-match" (macroexpand '(improper-elli (1 2 3 . 4)) 'strip) '(list (list 1 2 3) 4))

(macroexpand
 '(define-syntax vec-nest
    (syntax-rules ()
      ((_ #((a b) ...)) (list (list a b) ...)))))

(test "vec-nest-match" (macroexpand '(vec-nest #((1 2) (3 4))) 'strip) '(list (list 1 2) (list 3 4)))

(macroexpand
 '(define-syntax custom-elli
    (syntax-rules ::: ()
      ((_ a :::) (list a :::)))))

(test "custom-elli-match" (macroexpand '(custom-elli 1 2 3) 'strip) '(list 1 2 3))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
