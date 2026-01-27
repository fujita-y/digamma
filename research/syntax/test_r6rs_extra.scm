;; test_r6rs_extra.scm
;; Test suite for extra R6RS features like variable transformers.

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
;; Section 1: Variable Transformers
;; =============================================================================
(display "\n>>> Section 1: Variable Transformers\n")

(macroexpand
 '(define-syntax var-trans
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
         ((set! _ val) (syntax (display (list 'setting val))))
         ((_ . rest) (syntax (list 1 . rest)))
         (_ (syntax 1)))))))

(test "var-trans-ref" (macroexpand 'var-trans 'strip) 1)
(test "var-trans-call" (macroexpand '(var-trans 2 3) 'strip) '(list 1 2 3))
(test "var-trans-set" (macroexpand '(set! var-trans 10) 'strip) '(display (list 'setting 10)))

;; =============================================================================
;; Section 2: identifier-syntax
;; =============================================================================
(display "\n>>> Section 2: identifier-syntax\n")

(macroexpand
 '(define-syntax id-test
    (identifier-syntax 42)))

(test "id-syntax-ref" (macroexpand 'id-test 'strip) 42)
(test "id-syntax-call" (macroexpand '(id-test 1 2) 'strip) '(42 1 2))

(macroexpand
 '(define-syntax p.car
    (identifier-syntax
     (p.car (car p))
     ((set! p.car val) (set-car! p val)))))

(test "id-syntax-set-ref" (macroexpand 'p.car 'strip) '(car p))
(test "id-syntax-set-set" (macroexpand '(set! p.car 99) 'strip) '(set-car! p 99))

(macroexpand
 '(define-syntax nested-id
    (identifier-syntax
     (nested-id (inner-macro))
     ((set! nested-id val) (set-inner! val)))))

(test "nested-id-set" (macroexpand '(set! nested-id 7) 'strip) '(set-inner! 7))

;; =============================================================================
;; Section 3: free-identifier=? tests
;; =============================================================================
(display "\n>>> Section 3: free-identifier=? tests\n")

(macroexpand
 '(define-syntax check-else
    (lambda (x)
      (syntax-case x (else)
        ((_ else) (syntax 'is-else))
        ((_ x) (syntax 'not-else))))))

(test "free-id-else-match" (macroexpand '(check-else else) 'strip) ''is-else)
(test "free-id-else-non-match" (macroexpand '(check-else other) 'strip) ''not-else)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
