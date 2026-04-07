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
