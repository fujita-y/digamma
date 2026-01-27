(use gauche.test)
(use research.vm.compiler)
(use research.vm.vm)

(test-start "VM TCO")

(define (compile-and-run expr vm)
  (let ((code (compile expr)))
    (vm-load vm code)
    (vm-run vm)))

(define vm (make-vm))

;; Register primitives
(register-subr vm '- -)
(register-subr vm '= =)
;; Boxing support
(register-subr vm 'vector vector)
(register-subr vm 'vector-ref vector-ref)
(register-subr vm 'vector-set! vector-set!)

;; Deep recursion test
;; With TCO, this should run quickly and constant space.
;; Without TCO, this creates a large stack. In this interpreted VM context,
;; "large stack" is a long list in `vm-stack`.
;; We can inspect `vm-stack` depth in a custom subr or just try a huge number.

(register-subr vm 'check-stack 
               (lambda () 
                 (let ((depth (length (vm-stack vm))))
                   (print "Stack depth: " depth)
                   (if (> depth 10)
                       (error "Stack grew too deep! TCO failed.")
                       #t))))

(test-section "Tail Recursion")

(test* "Tail Sum (simulated loop)" #t
       (compile-and-run 
        '(let ((loop #f))
           (set! loop (lambda (n)
                        (if (= n 0)
                            #t
                            (begin
                              (check-stack) ;; Assert stack doesn't grow
                              (loop (- n 1))))))
           (loop 100)) ;; 100 iterations is enough to detect stack growth > 10
        vm))

(test-end)
