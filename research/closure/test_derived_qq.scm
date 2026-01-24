(load "derived_syntax.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-desugar-qq name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Desugared: ") (pretty-print (expand-derived-syntax expr))
  (newline))

;; Test quasiquote expansion through derived_syntax
(test-desugar-qq "Quasiquote Simple"
  '(quasiquote (a b c)))

(test-desugar-qq "Quasiquote with Unquote"
  '(quasiquote (a (unquote x) b)))

(test-desugar-qq "Quasiquote with Unquote-Splicing"
  '(quasiquote (a (unquote-splicing x) b)))

(test-desugar-qq "Quasiquote in Lambda"
  '(lambda (x) (quasiquote (result (unquote x)))))

(test-desugar-qq "Combined: Let* and Quasiquote"
  '(let* ((x 1) (y 2))
     (quasiquote (values (unquote x) (unquote y)))))
