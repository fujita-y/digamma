(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (core-eval (macroexpand expr 'strip) (interaction-environment))))
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

(define-module (core parameterize) 
(export parameterize)
(begin

  (define-syntax parameterize-aux
    (syntax-rules ()
      ((_ () ((save new param value) ...) body ...)
      (let ((save #f) ... (new value) ...)
          (dynamic-wind
          (lambda () (set! save (param)) ... (param new) ...)
          (lambda () body ...)
          (lambda () (param save) ...))))
      ((_ ((e1 e2) . more) (stash ...) body ...)
      (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ ((e1 e2) ...) body ...)
      (parameterize-aux ((e1 e2) ...) () body ...))))

))

(import-module (core parameterize))

(define p (make-parameter 1))

(test "parameterize" 
      '(parameterize ((p 2)) (p)) 
      2)

;;;;;

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))

#|
./build/nanos --boot boot/core.ir --script tests/test-module-param.scm

shows error:

[codegen] Unknown global or letrec closure: parameterize-aux

It seems hygiene issue with (core parameterize) macro.

Fix it

use 'gosh boot/build-core-ir.scm' to rebuilt boot/core.ir before test changes with ./build/nanos

|#