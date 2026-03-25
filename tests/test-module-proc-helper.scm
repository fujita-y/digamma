(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (core-eval (macroexpand expr) (current-environment))))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

;;;;;

(define (helper x y) (+ x y 100))

(define-module (tests foo)
  (export foo-macro)
  (begin
    (define (helper x) (+ x 1))
    (define-syntax foo-macro
      (lambda (stx) 
        (syntax-case stx ()
         ((_ x) #'(helper x)))))))

(define-module (tests bar)
  (export bar-proc)
  (import (tests foo))
  (begin
    (define (bar-proc x) (foo-macro x))))

(import-module (tests bar))

(test "helper" (bar-proc 10) 11)

;;;;;

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))


#|
./build/nanos --boot boot/core.ir --script tests/test-module-proc-helper.scm
|#