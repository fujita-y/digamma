
(use srfi-1)
(load "research/vm/vm.scm")
(load "research/vm/compiler.scm")

(define (test-multi-context)
  (display "Testing multiple contexts in one VM...\n")
  (let* ((vm (vm:init-vm))
         ;; Context 1: Define a global variable
         (code1 (cp:compile '(define x 100)))
         (ctx1 (vm:init-context vm code1))
         ;; Context 2: Reference that global variable
         (code2 (cp:compile 'x))
         (ctx2 (vm:init-context vm code2))
         ;; Context 3: Update the global variable
         (code3 (cp:compile '(set! x (+ x 50))))
         (ctx3 (vm:init-context vm code3))
         ;; Context 4: Verifying the update
         (code4 (cp:compile 'x))
         (ctx4 (vm:init-context vm code4)))

    (vm:vm-set-global! vm '+ +)

    (let ((res1 (vm:vm-run ctx1)))
      (format #t "Ctx1 defined x: ~s\n" res1))

    (let ((res2 (vm:vm-run ctx2)))
      (format #t "Ctx2 read x: ~s\n" res2)
      (if (not (= res2 100)) (error "Failed: Ctx2 should see x=100")))

    (let ((res3 (vm:vm-run ctx3)))
      (format #t "Ctx3 updated x: ~s\n" res3))

    (let ((res4 (vm:vm-run ctx4)))
      (format #t "Ctx4 read updated x: ~s\n" res4)
      (if (not (= res4 150)) (error "Failed: Ctx4 should see x=150")))

    (display "Multi-context shared state test PASSED!\n")))

(test-multi-context)
