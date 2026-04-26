;; ==== Combined test file ====
(define *pass-count* 0)
(define *fail-count* 0)

;; ==== from test-fiber.scm ====
;; tests/test-fiber.scm

(define (test name output expected)
  (if (equal? output expected)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

(display "\n>>> Fiber Tests\n")

;; Test 1: Basic fiber execution and result
(let* ((f (fiber (lambda () (+ 1 2))))
       (result (future-get f)))
  (test "Basic fiber execution" result 3))

;; Test 2: Fiber fiber-yielding
(let ((order '()))
  (let* ((f1 (fiber (lambda ()
                      (set! order (append order '(1)))
                      (fiber-yield)
                      (set! order (append order '(3)))
                      'done1)))
         (f2 (fiber (lambda ()
                      (set! order (append order '(2)))
                      (fiber-yield)
                      (set! order (append order '(4)))
                      'done2))))
    (future-get f1)
    (future-get f2)
    (test "Fiber fiber-yield order" order '(1 2 3 4))))

;; Test 3: Multiple fibers and futures
(let* ((v (make-vector 10 0))
       (fibers (map (lambda (i)
                      (fiber (lambda ()
                               (vector-set! v i (* i i))
                               i)))
                    '(0 1 2 3 4 5 6 7 8 9)))
       (results (map future-get fibers)))
  (test "Multiple fibers results" results '(0 1 2 3 4 5 6 7 8 9))
  (test "Multiple fibers side effects" v '#(0 1 4 9 16 25 36 49 64 81)))

;; Test 4: Nested fibers
(let* ((f1 (fiber (lambda ()
                    (let* ((f2 (fiber (lambda () "inner")))
                           (res (future-get f2)))
                      (string-append "outer-" res)))))
       (result (future-get f1)))
  (test "Nested fibers" result "outer-inner"))

;; Test 5: fiber-sleep-for
(let* ((start (time-usage))
       (res (fiber-sleep-for 100))
       (end (time-usage)))
  (test "fiber-sleep-for result" res (unspecified))
  (test "fiber-sleep-for duration" (>= (- (car end) (car start)) 0.1) #t))

;; Test 6: future-wait-for
(let* ((f (fiber (lambda () (fiber-sleep-for 200) 'done)))
       (timeout1 (future-wait-for f 100))
       (timeout2 (future-wait-for f 200)))
  (test "future-wait-for timeout" timeout1 #t)
  (test "future-wait-for ready" timeout2 #f))


;; Test 8: Multiple future-get calls
(let* ((f (fiber (lambda () 'result)))
       (r1 (future-get f))
       (r2 (future-get f)))
  (test "Multiple future-get 1" r1 'result)
  (test "Multiple future-get 2" r2 'result))

;; Test 9: GC Survival (External object)
(let* ((v (make-vector 100 42))
       (f (fiber (lambda ()
                   (fiber-yield)
                   (collect)
                   (safepoint)
                   (safepoint)
                   (safepoint)
                   (fiber-yield)
                   (vector-ref v 50)))))
  (collect)
  (safepoint)
  (safepoint)
  (safepoint)
  (test "GC survival (external object)" (future-get f) 42))

;; Test 10: GC Survival (Internal object)
(let* ((f (fiber (lambda ()
                   (let ((v (make-vector 100 99)))
                     (fiber-yield)
                     (collect)
                     (safepoint)
                     (safepoint)
                     (safepoint)
                    (fiber-yield)
                     (vector-ref v 50))))))
  (test "GC survival (internal object)" (future-get f) 99))

;; Test 11: GC Survival (Fiber stack roots)
(let* ((f (fiber (lambda ()
                   (let ((lst (list 1 2 3 4 5)))
                     (fiber-yield)
                     (collect)
                     (safepoint)
                     (safepoint)
                     (safepoint)
                     (apply + lst))))))
  (collect)
  (safepoint)
  (safepoint)
  (safepoint)
  (test "GC survival (fiber stack roots)" (future-get f) 15))

;; Test 12: GC Survival (Multiple yielding fibers)
(let* ((fibers (map (lambda (i)
                      (fiber (lambda ()
                               (let ((v (make-vector 100 i)))
                                 (fiber-yield)
                                 (collect)
                                 (safepoint)
                                 (safepoint)
                                 (safepoint)
                                 (fiber-yield)
                                 (vector-ref v i)))))
                    '(0 1 2 3 4 5 6 7 8 9)))
       (results (map future-get fibers)))
  (test "GC survival (multiple fibers)" results '(0 1 2 3 4 5 6 7 8 9)))

;; Summary
(newline)

;; ==== from test_write_ss.scm ====
; test_write_ss.scm — SRFI-38 write/ss tests

(define (check label result expected)
  (if (string=? result expected)
      (begin (display "PASS: ") (display label) (newline))
      (begin (display "FAIL: ") (display label)
             (display " got: ") (write result)
             (display " expected: ") (write expected) (newline))))

(define (capture-write-ss expr)
  (call-with-values
    open-string-output-port
    (lambda (port extract)
      (write/ss expr port)
      (extract))))

; Test 1: simple list
(check "simple list" (capture-write-ss '(1 2 3)) "(1 2 3)")

; Test 2: vector no sharing
(check "vector no sharing" (capture-write-ss '#(1 2 3)) "#(1 2 3)")

; Test 3: shared cons
(let ((x (list 1 2)))
  (check "shared cons" (capture-write-ss (list x x)) "(#0=(1 2) #0#)"))

; Test 4: shared cons in vector
(let ((x (list 'a 'b)))
  (check "shared in vector" (capture-write-ss (vector x x)) "#(#0=(a b) #0#)"))

; Test 5: circular list (1 . #0#)
(let ((x (list 1)))
  (set-cdr! x x)
  (check "circular list" (capture-write-ss x) "#0=(1 . #0#)"))

; Test 6: shared string
(let ((s "hello"))
  (check "shared string" (capture-write-ss (list s s)) "(#0=\"hello\" #0#)"))

(display "done") (newline)



(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
