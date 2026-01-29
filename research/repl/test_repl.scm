;;; Test for REPL logic
(load "./research/repl/repl.scm")

(define (test-repl-expr expr expected)
  (format #t "Testing: ~s\n" expr)
  (let* ((expanded (macroexpand expr 'strip))
         (optimized (op:optimize expanded))
         (code (cp:compile optimized))
         (vm (vm:init-vm code)))
    (repl:init-globals vm)
    (let ((result (vm:vm-run vm)))
      (if (equal? result expected)
          (format #t "  PASS: ~s\n" result)
          (begin
            (format #t "  FAIL: expected ~s, got ~s\n" expected result)
            (exit 1))))))

(display "Running REPL integration tests...\n")

(test-repl-expr '(+ 1 2) 3)
(test-repl-expr '(let ((x 10)) (+ x 20)) 30)
(test-repl-expr '(define-syntax foo (syntax-rules () ((foo x) (+ x 1)))) 'defined) ; Wait, define-syntax returns 'defined
(test-repl-expr '(let-syntax ((foo (syntax-rules () ((foo x) (+ x 1))))) (foo 10)) 11)
(test-repl-expr '(if (= 0 0) 'yes 'no) 'yes)
(test-repl-expr '((lambda (x) (* x x)) 5) 25)

(display "All REPL integration tests PASSED!\n")
