;; ==== Combined System & Runtime Tests ====
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (core-eval expr (current-environment))))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

(define (check name result expected)
  (if (equal? result expected)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write result) (newline))))

;; =============================================================================
;; Section 1: Fibers & Concurrency
;; =============================================================================
(display "\n>>> Section 1: Fibers & Concurrency\n")

(test "fiber-basic"
      '(let ((f (fiber (lambda () 42))))
         (future-get f))
      42)

(test "fiber-yield"
      '(let ((f (fiber (lambda () 
                         (fiber-yield)
                         42))))
         (future-get f))
      42)

(test "fiber-sleep-for"
      '(let ((f (fiber (lambda () 
                         (fiber-sleep-for 10)
                         42))))
         (future-get f))
      42)

(test "future-wait-for"
      '(let ((f (fiber (lambda () 42))))
         (future-wait-for f 1000))
      #f)

;; Missing Test: future?
(test "future? (true)"
      '(future? (fiber (lambda () 42)))
      #t)

(test "future? (false)"
      '(future? 42)
      #f)

;; Missing Test: future-wait
(test "future-wait"
      '(let ((f (fiber (lambda () 42))))
         (future-wait f)
         (future-get f))
      42)

;; Complex Fiber interaction
(define f1 
  (fiber 
    (lambda () 
      (display "Fiber 1 started\n")
      (fiber-sleep-for 50)
      (display "Fiber 1 resuming\n")
      10)))

(define f2 
  (fiber 
    (lambda () 
      (display "Fiber 2 started\n")
      (let ((v (future-get f1)))
        (display "Fiber 2 got value from Fiber 1: ") (display v) (newline)
        (* v 2)))))

(check "Fiber interaction" (future-get f2) 20)

;; =============================================================================
;; Section 2: Object Serialization (write/ss)
;; =============================================================================
(display "\n>>> Section 2: Object Serialization (write/ss)\n")

(define (call-with-output-string proc)
  (call-with-values
    (lambda () (open-string-output-port))
    (lambda (port extract)
      (proc port)
      (extract))))

(check "srfi-38 basic" (call-with-output-string (lambda (p) (write/ss '(1 2 3) p))) "(1 2 3)")
(check "srfi-38 shared 1" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2))) (list x x)) p))) "(#0=(1 2) #0#)")
(check "srfi-38 shared 2" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2)) (y (list 3 4))) (list x y x y)) p))) "(#0=(1 2) #1=(3 4) #0# #1#)")

(check "srfi-38 shared 3" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1))) (set-car! x x) x) p))) "#0=(#0#)")

(check "srfi-38 shared 4" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2))) (set-cdr! (cdr x) x) x) p))) "#0=(1 2 . #0#)")

(check "srfi-38 shared 5" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2 3))) (set-cdr! (cddr x) x) x) p))) "#0=(1 2 3 . #0#)")

(check "srfi-38 shared 6" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2 3 4))) (set-cdr! (cddr x) (cddr x)) x) p))) "(1 2 . #0=(3 . #0#))")

(check "srfi-38 shared 7" (call-with-output-string (lambda (p) (write/ss (let ((x (list 1 2 3 4))) (set-car! (cdddr x) x) x) p))) "#0=(1 2 3 #0#)")

;; Missing Test: Circular vector
(test "circular vector write/ss"
      '(let ((v (vector 1 2 3)))
         (vector-set! v 1 v)
         (call-with-output-string (lambda (p) (write/ss v p))))
      "#0=#(1 #0# 3)")

;; =============================================================================
;; Section 3: Escape Continuations
;; =============================================================================
(display "\n>>> Section 3: Escape Continuations\n")

(test "call/ec basic"
      '(call/ec (lambda (k) 42))
      42)

(test "call/ec escape"
      '(call/ec (lambda (k) (k 42) 99))
      42)

(test "call/ec nested"
      '(call/ec (lambda (k1)
                  (call/ec (lambda (k2)
                             (k1 42)
                             99))))
      42)

(test "call/ec return procedure"
      '(procedure? (call/ec (lambda (k) k)))
      #t)

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
