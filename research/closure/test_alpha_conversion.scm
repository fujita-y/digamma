(load "alpha_conversion.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-alpha-conversion name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Converted: ") (pretty-print (alpha-conversion expr))
  (newline))

(test-alpha-conversion "Identity"
  '(lambda (x) x))

(test-alpha-conversion "Shadowing"
  '(lambda (x) (lambda (x) x)))

(test-alpha-conversion "Let Shadowing"
  '(let ((x 1)) (let ((x 2)) x)))

(test-alpha-conversion "Define Function 1"
  '(define (add x y) (+ x y)))

(test-alpha-conversion "Define Function 2"
  '(define add (lambda (x y) (+ x y))))

(test-alpha-conversion "Free Variables"
  '(lambda (x) (+ x y))) ; y is free, should behave as is or error? Logic says returns 'y'

(test-alpha-conversion "Complex Nested"
  '(define (foo a)
     (let ((b (+ a 1)))
       (lambda (c)
         (let ((a (* c 2))) ;; Shadows outer 'a'
           (+ a b))))))
