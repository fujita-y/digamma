(display "Testing put-char and put-string...")
(newline)

(define (test-it)
  (define (check name expected actual)
    (if (equal? expected actual)
        (begin (display "PASS: ") (display name) (newline))
        (begin
          (display "FAIL: ") (display name) (newline)
          (display "  expected: ") (write expected) (newline)
          (display "  actual:   ") (write actual) (newline)
          (error 'test "failed"))))

  (call-with-values (lambda () (open-string-output-port))
    (lambda (p extract)
      (put-char p #\a)
      (put-char p #\b)
      (put-char p #\c)
      (check "put-char basic" "abc" (extract))))

  (call-with-values (lambda () (open-string-output-port))
    (lambda (p extract)
      (put-string p "hello")
      (check "put-string basic" "hello" (extract))))

  (call-with-values (lambda () (open-string-output-port))
    (lambda (p extract)
      (put-string p "hello world" 6)
      (check "put-string start" "world" (extract))))

  (call-with-values (lambda () (open-string-output-port))
    (lambda (p extract)
      (put-string p "hello world" 0 5)
      (check "put-string start/count" "hello" (extract))))

  (call-with-values (lambda () (open-string-output-port))
    (lambda (p extract)
      (put-char p #\λ)
      (check "put-char unicode" "λ" (extract))))
  
  (display "All put tests passed!")
  (newline))

(test-it)
