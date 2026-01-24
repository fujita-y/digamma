(load "closure_conversion.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (run-test name expr)
  (display ";; ------------------------------------------")
  (newline)
  (display ";; Test Case: ")
  (display name)
  (newline)
  (display ";; Input:")
  (newline)
  (pretty-print expr)
  (newline)
  (display ";; Output:")
  (newline)
  (let ((result (closure-conversion expr)))
    (for-each pretty-print result))
  (newline))

;; Case 0: No capture
(run-test "Case 0 (No capture)"
  '(define foo
     (lambda (lst)
       (map (lambda (x) (+ x 3)) lst))))


;; Case 1: Simple capture
(run-test "Case 1 (from memo.txt)"
  '(define foo
     (lambda (val lst)
       (map (lambda (x) (+ x val)) lst))))

;; Case 2: Mutablity
(run-test "Case 2 (from memo.txt)"
  '(define foo
     (lambda (val lst)
       (cons (map (lambda (x) (set! val (- val 1)) (+ x val)) lst)
             val))))

;; Case 3: Nested Lambdas
(run-test "Case 3 (Nested Lambdas)"
  '(define nesting
     (lambda (x)
       (lambda (y)
         (lambda (z)
           (+ x (+ y z)))))))

;; Case 4: Multiple Arguments in Inner Lambda
(run-test "Case 4 (Multiple Args)"
  '(define multi
     (lambda (a)
       (lambda (b c)
         (+ a (+ b c))))))
