(load "quasiquote.scm")

(define (pretty-print expr)
  (write expr)
  (newline))

(define (test-qq name expr)
  (display "---------------------------------------------------")
  (newline)
  (display "Test: ") (display name) (newline)
  (display "Original: ") (pretty-print expr)
  (display "Expanded: ") (pretty-print (qq-expand expr))
  (newline))

;; Basic quasiquote
(test-qq "Simple Quote"
  '(quasiquote x))

(test-qq "Quote List"
  '(quasiquote (a b c)))

;; Unquote
(test-qq "Simple Unquote"
  '(quasiquote (unquote x)))

(test-qq "Unquote in List"
  '(quasiquote (a (unquote x) b)))

(test-qq "Multiple Unquotes"
  '(quasiquote ((unquote x) (unquote y))))

;; Unquote-splicing
(test-qq "Simple Unquote-Splicing"
  '(quasiquote ((unquote-splicing x))))

(test-qq "Unquote-Splicing in Middle"
  '(quasiquote (a (unquote-splicing x) b)))

(test-qq "Unquote-Splicing at Start"
  '(quasiquote ((unquote-splicing x) b c)))

(test-qq "Unquote-Splicing at End"
  '(quasiquote (a b (unquote-splicing x))))

;; Mixed
(test-qq "Mixed Unquote and Splicing"
  '(quasiquote (a (unquote x) (unquote-splicing y) b)))

;; Nested structures
(test-qq "Nested List"
  '(quasiquote (a (b (unquote x) c) d)))

(test-qq "Vector (treated as list)"
  '(quasiquote (a b c)))

;; Nested quasiquote
(test-qq "Nested Quasiquote"
  '(quasiquote (a (quasiquote (b (unquote x))))))

(test-qq "Double Nested Quasiquote"
  '(quasiquote (quasiquote (unquote (unquote x)))))

;; Edge cases
(test-qq "Empty List"
  '(quasiquote ()))

(test-qq "Dotted Pair"
  '(quasiquote (a . b)))

(test-qq "Dotted with Unquote"
  '(quasiquote (a . (unquote b))))

;; Real-world examples
(test-qq "Lambda Template"
  '(quasiquote (lambda (unquote params) (unquote-splicing body))))

(test-qq "Let Template"
  '(quasiquote (let ((unquote var) (unquote val)) (unquote-splicing body))))

(test-qq "List Construction"
  '(quasiquote (list (unquote a) (unquote b) (unquote c))))
