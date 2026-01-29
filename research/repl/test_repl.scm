;;; Test for REPL logic
(load "./research/repl/repl.scm")

(define (test-repl-expr expr expected)
  (format #t "Testing: ~s\n" expr)
  (let* ((expanded (macroexpand expr 'strip))
         (optimized (op:optimize expanded))
         (code (cp:compile optimized))
         (vm (vm:init-vm))
         (ctx (vm:init-context vm code)))
    (repl:init-globals vm)
    (let ((result (vm:vm-run ctx)))
      (if (equal? result expected)
          (format #t "  PASS: ~s\n" result)
          (begin
            (format #t "  FAIL: expected ~s, got ~s\n" expected result)
            (exit 1))))))

(display "Running REPL integration tests...\n")

(test-repl-expr '(+ 1 2) 3)
(test-repl-expr '(let ((x 10)) (+ x 20)) 30)
(test-repl-expr '(define-syntax foo (syntax-rules () ((foo x) (+ x 1)))) 'defined)
(test-repl-expr '(let-syntax ((foo (syntax-rules () ((foo x) (+ x 1))))) (foo 10)) 11)
(test-repl-expr '(if (= 0 0) 'yes 'no) 'yes)
(test-repl-expr '((lambda (x) (* x x)) 5) 25)

;;; Pitfall Tests
(display "Running Pitfall tests...\n")

;; 1. Shared state across multiple contexts in the same VM
(let* ((vm (vm:init-vm))
       (_ (repl:init-globals vm))
       (run (lambda (expr)
              (let* ((expanded (macroexpand expr 'strip))
                     (optimized (op:optimize expanded))
                     (code (cp:compile optimized))
                     (ctx (vm:init-context vm code)))
                (vm:vm-run ctx)))))
  (display "  Testing shared state across contexts...\n")
  (run '(define x 42))
  (let ((res (run 'x)))
    (if (equal? res 42)
        (display "    PASS: shared global x=42\n")
        (begin (format #t "    FAIL: expected 42, got ~s\n" res) (exit 1))))
  (run '(set! x 100))
  (let ((res (run 'x)))
    (if (equal? res 100)
        (display "    PASS: shared global x updated to 100\n")
        (begin (format #t "    FAIL: expected 100, got ~s\n" res) (exit 1)))))

;; 2. Closure mutation (capturing a cell and mutating it)
(test-repl-expr '(let ((x 10))
                   (let ((f (lambda () (set! x (+ x 1)) x)))
                     (f)
                     (f)
                     (f)))
                13)

;; 3. Shadowing
(test-repl-expr '(let ((x 10))
                   (let ((x 20))
                     (+ x x)))
                40)

(test-repl-expr '(begin
                   (define x 100)
                   (let ((x 10))
                     x))
                10)

;; 4. Mutual Recursion
(test-repl-expr '(begin
                   (define e (lambda (n) (if (= n 0) #t (o (- n 1)))))
                   (define o (lambda (n) (if (= n 0) #f (e (- n 1)))))
                   (e 10))
                #t)

(display "All REPL integration and pitfall tests PASSED!\n")
