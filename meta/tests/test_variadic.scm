(use srfi-1)
(use srfi-9)

(load "../vm.scm")
(load "../../core/compiler.scm")

(define (test name expr expected)
  (format #t "Testing ~a: ~s\n" name expr)
  (let* ((code (cp:compile expr))
         (vm (vm:init-vm))
         (ctx (vm:init-context vm code)))
    (vm:vm-set-global! vm '+ +)
    (vm:vm-set-global! vm '- -)
    (vm:vm-set-global! vm '* *)
    (vm:vm-set-global! vm 'list list)
    (vm:vm-set-global! vm 'equal? equal?)
    
    (let ((result (let loop ((res (vm:vm-run ctx))) res)))
      (if (equal? result expected)
          (format #t "  PASS: ~s\n" result)
          (begin
            (format #t "  FAIL: expected ~s, got ~s\n" expected result)
            (exit 1))))))

(test "Fixed args (2)" '((lambda (x y) (+ x y)) 10 20) 30)

;; Variadic tests - these should currently fail to compile or run correctly
(test "Rest arg only (0 args)" '((lambda x x)) '())
(test "Rest arg only (1 arg)" '((lambda x x) 1) '(1))
(test "Rest arg only (3 args)" '((lambda x x) 1 2 3) '(1 2 3))

(test "Mixed args (2 args)" '((lambda (x y . z) (list x y z)) 1 2) '(1 2 ()))
(test "Mixed args (3 args)" '((lambda (x y . z) (list x y z)) 1 2 3) '(1 2 (3)))
(test "Mixed args (4 args)" '((lambda (x y . z) (list x y z)) 1 2 3 4) '(1 2 (3 4)))

(print "All variadic tests PASSED!")
