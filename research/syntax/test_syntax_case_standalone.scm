;; test_syntax_case_standalone.scm
(load "./syntax_case.scm")

(define (test name output expected)
  (display name)
  (if (equal? output expected)
      (display " ... PASS\n")
      (begin
        (display " ... FAIL\n")
        (display "  Expected: ") (write expected) (newline)
        (display "  Got:      ") (write output) (newline))))

;; Test basic matching
(let* ((input (make-syntax-object '(foo 1 2 3) '()))
       (result (expand-syntax-case input '() 
                '(((name val ...) #t (syntax (name val ...)))) 
                (interaction-environment))))
  (test "basic-syntax-case" (syntax->datum result) '(foo 1 2 3)))

;; Test ellipsis expansion
(let* ((input (make-syntax-object '(test-let ((x 1) (y 2)) + x y) '()))
       (result (expand-syntax-case input '()
                '(((test-let ((var val) ...) body ...) #t (syntax (list (list 'var val) ... 'body ...))))
                (interaction-environment))))
  (test "ellipsis-expansion" (syntax->datum result) '(list (list 'x 1) (list 'y 2) '+ 'x 'y)))

;; Test fenders
(let* ((input (make-syntax-object '(foo 1 2 3) '()))
       (result (expand-syntax-case input '()
                '(((name val ...) (null? (syntax->datum (syntax (val ...)))) 'empty)
                  ((name val ...) #t 'not-empty))
                (interaction-environment))))
  (test "fender-false" result 'not-empty))

(display "Done.\n")
