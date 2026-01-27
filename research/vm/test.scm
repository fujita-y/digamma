(use gauche.test)
(use research.vm.compiler)
(use research.vm.vm)

(test-start "VM and Compiler")

(define (compile-and-run expr vm)
  (let ((code (compile expr)))
    (print "Compiled code: " code)
    (vm-load vm code)
    (vm-run vm)))

(define vm (make-vm))

;; Register primitives
(register-subr vm '+ +)
(register-subr vm '- -)
(register-subr vm '* *)
(register-subr vm '= =)
(register-subr vm 'cons cons)
(register-subr vm 'car car)
(register-subr vm 'cdr cdr)
(register-subr vm 'null? null?)
(register-subr vm 'vector vector)
(register-subr vm 'vector-ref vector-ref)
(register-subr vm 'vector-set! vector-set!)

(test-section "Basic Expressions")
(test* "Constant integer" 42 (compile-and-run 42 vm))
(test* "Constant boolean" #t (compile-and-run #t vm))
(test* "Quote" 'foo (compile-and-run ''foo vm))

(test-section "Primitives")
(test* "Add" 3 (compile-and-run '(+ 1 2) vm))
(test* "Nested Add" 6 (compile-and-run '(+ 1 (+ 2 3)) vm))

(test-section "Conditionals")
(test* "If true" 1 (compile-and-run '(if #t 1 2) vm))
(test* "If false" 2 (compile-and-run '(if #f 1 2) vm))

(test-section "Variables and Let")
(test* "Let simple" 3 (compile-and-run '(let ((x 1) (y 2)) (+ x y)) vm))
(test* "Let shadowing" 4 (compile-and-run '(let ((x 1)) (let ((x 4)) x)) vm))

(test-section "Lambda and Application")
(test* "Identity" 5 (compile-and-run '((lambda (x) x) 5) vm))
(test* "Simple closure" 3 (compile-and-run '((lambda (x) (+ x 1)) 2) vm))
(test* "Zero args" 10 (compile-and-run '((lambda () 10)) vm))

(test-section "Closures (Free variables)")
(test* "Make adder" 15
       (compile-and-run '((let ((x 10))
                            (lambda (y) (+ x y))) 5) vm))

(test* "Nested closure" 25
       (compile-and-run '(let ((x 10))
                            (let ((f (lambda (y) (+ x y))))
                              (let ((g (lambda (z) (+ (f z) x))))
                                (g 5)))) vm))
;; Correction: 25.

(test* "Currying" 3
       (compile-and-run '(((lambda (x) (lambda (y) (+ x y))) 1) 2) vm))

(test-section "Globals and Definition")
(compile-and-run '(define gl 100) vm)
(test* "Global Ref" 100 (compile-and-run 'gl vm))
(compile-and-run '(set! gl 200) vm)
(test* "Global Set" 200 (compile-and-run 'gl vm))

(test-section "Recursion")
(test* "Factorial" 120
       (compile-and-run '(let ((fact #f))
                           (set! fact (lambda (n)
                                        (if (= n 0)
                                            1
                                            (* n (fact (- n 1))))))
                           (fact 5)) vm))

(test-end)
