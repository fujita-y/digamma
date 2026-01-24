(load "derived_syntax.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-desugar name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Desugared: ") (pretty-print (expand-derived-syntax expr))
  (newline))

;; Test let*
(test-desugar "let* Simple"
  '(let* ((x 1)) x))

(test-desugar "let* Two Bindings"
  '(let* ((x 1) (y 2)) (+ x y)))

(test-desugar "let* Sequential Dependency"
  '(let* ((x 10) (y (+ x 5))) y))

(test-desugar "let* Empty Bindings"
  '(let* () 42))

;; Test letrec
(test-desugar "letrec Simple"
  '(letrec ((fact (lambda (n)
                    (if (= n 0)
                        1
                        (* n (fact (- n 1)))))))
     (fact 5)))

(test-desugar "letrec Mutual Recursion"
  '(letrec ((even? (lambda (n)
                     (if (= n 0) #t (odd? (- n 1)))))
            (odd? (lambda (n)
                    (if (= n 0) #f (even? (- n 1))))))
     (even? 10)))

;; Test letrec*
(test-desugar "letrec* Simple"
  '(letrec* ((x 1) (y 2)) (+ x y)))

;; Test cond
(test-desugar "cond Simple"
  '(cond ((< x 0) 'negative)
         ((= x 0) 'zero)
         (else 'positive)))

(test-desugar "cond Without Else"
  '(cond ((< x 0) 'negative)
         ((= x 0) 'zero)))

(test-desugar "cond Single Test"
  '(cond (x 'truthy)))

(test-desugar "cond Test Only (returns test value)"
  '(cond (x)
         (else 'false)))

;; Test and
(test-desugar "and Empty"
  '(and))

(test-desugar "and Single"
  '(and x))

(test-desugar "and Multiple"
  '(and a b c))

;; Test or
(test-desugar "or Empty"
  '(or))

(test-desugar "or Single"
  '(or x))

(test-desugar "or Multiple"
  '(or a b c))

;; Test case
(test-desugar "case Simple"
  '(case x
     ((1) 'one)
     ((2) 'two)
     (else 'other)))

(test-desugar "case Multiple Values"
  '(case x
     ((1 2 3) 'small)
     ((4 5 6) 'medium)
     (else 'large)))

;; Test nested transformations
(test-desugar "Nested let* and cond"
  '(let* ((x 10)
          (y 20))
     (cond ((< x y) (+ x y))
           (else (- x y)))))

(test-desugar "Lambda with let*"
  '(lambda (n)
     (let* ((x (* n 2))
            (y (+ x 1)))
       y)))
