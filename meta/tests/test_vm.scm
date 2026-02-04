



(load "../../core/core.scm")
(load "../vm.scm")

(define (test name expr expected)
  (format #t "Testing ~a: ~s\n" name expr)
  (let* ((code (compile expr))
         (ctx (vm:init-context code)))
    ;; Add some basic subrs to globals
    (global-variable-set! '+ +)
    (global-variable-set! '- -)
    (global-variable-set! '* *)
    (global-variable-set! '/ /)
    (global-variable-set! '= =)
    (global-variable-set! 'list list)
    (global-variable-set! 'cons cons)
    (global-variable-set! 'car car)
    (global-variable-set! 'cdr cdr)
    (global-variable-set! 'null? null?)
    (global-variable-set! 'display display)
    (global-variable-set! 'newline newline)


    (let ((result (vm:vm-run ctx)))
      (if (equal? result expected)
          (format #t "  PASS: ~s\n" result)
          (begin
            (format #t "  FAIL: expected ~s, got ~s\n" expected result)
            (for-each (lambda (i) (print i)) (vector->list code))
            (exit 1))))))

(test "Simple quote" '(quote 42) 42)
(test "Simple begin" '(begin 1 2 3) 3)
(test "Simple if" '(if #t 1 2) 1)
(test "Simple if false" '(if #f 1 2) 2)
(test "Simple let" '(let ((a 10) (b 20)) (+ a b)) 30)
(test "Nested let" '(let ((a 10)) (let ((b 20)) (+ a b))) 30)

(test "Lambda call" '((lambda (x) (* x x)) 10) 100)

(test "Closure" '(let ((a 10))
                   (let ((f (lambda (x) (+ x a))))
                     (f 20)))
      30)

(test "Mutable variable (set!)" '(let ((a 10))
                                   (set! a 20)
                                   a)
      20)

(test "Mutable free variable" '(let ((a 10))
                                 (let ((inc (lambda () (set! a (+ a 1)) a)))
                                   (inc)
                                   (inc)
                                   (inc)))
      13)

(test "Tail call" '(begin
                     (define fact (lambda (n acc)
                                    (if (= n 0) acc
                                        (fact (- n 1) (* n acc)))))
                     (fact 5 1))
      120)

(test "Recursive map" '(begin
                         (define map (lambda (f l)
                                       (if (null? l) '()
                                           (cons (f (car l)) (map f (cdr l))))))
                         (map (lambda (x) (* x x)) '(1 2 3)))
      '(1 4 9))

(test "Mutual Recursion (even?/odd?)" '(begin
                                        (define even? (lambda (n)
                                                       (if (= n 0) #t
                                                           (odd? (- n 1)))))
                                        (define odd? (lambda (n)
                                                      (if (= n 0) #f
                                                          (even? (- n 1)))))
                                        (even? 10))
      #t)

(test "Many registers (r32+)"
      '(let ((x1 1) (x2 2) (x3 3) (x4 4) (x5 5))
         (+ x1 (+ x2 (+ x3 (+ x4 x5)))))
      15)

(test "Many registers (r32+)"
      '(let ((p (lambda (x1 x2 x3 x4 x5 x6 x7 x8) (+ x1 x2 x3 x4 x5 x6 x7 x8))))
         (p 1 2 3 4 5 6 7 8))
      36)

(test "Recursive closure (closure-self)"
      '(let ((loop #f))
         (set! loop (lambda (n)
                      (let ((inner (lambda () (loop (- n 1)))))
                        (if (= n 0) 'done (inner)))))
         (loop 5))
      'done)

(print "All basic tests PASSED!")
