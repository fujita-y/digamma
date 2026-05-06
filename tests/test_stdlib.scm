;; ==== Standard Library Tests ====
(define *pass-count* 0)
(define *fail-count* 0)

(import (core))

(define (test name expr expected)
  (let ((result expr))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

;; =============================================================================
;; Section 1: destructuring-match
;; =============================================================================
(display "\n>>> Section 1: destructuring-match\n")

(test "simple match"
      (destructuring-match '(1 2 3) ((a b c) (+ a b c)))
      6)

(test "quote match"
      (destructuring-match '(quote 1) (('quote e) (list 1)) (_ (list 'nomatch)))
      '(1))

(test "ellipsis match"
      (destructuring-match '(1 2 3 4 5) ((a b ... c) (list a b c)))
      '(1 (2 3 4) 5))

(test "nested lambda call"
      (destructuring-match '((lambda (x y) (+ x y)) 1 2)
        ((('lambda (vars ...) . body) . args)
         (and (= (length vars) (length args))
              (list vars body args))))
      '((x y) ((+ x y)) (1 2)))

(test "if #t match"
      (destructuring-match '(#t 1 2)
        ((#t e1 . _) e1))
      1)

(test "if #f match"
      (destructuring-match '(#f 1 2)
        ((#f _ . e2) e2))
      '(2))

(test "literal match"
      (destructuring-match '(not x)
        (('not e1) e1))
      'x)

(test "fender match"
      (destructuring-match '(1 2)
        ((a b) (= a b) 'equal)
        ((a b) 'not-equal))
      'not-equal)

(test "fender match 2"
      (destructuring-match '(1 1)
        ((a b) (= a b) 'equal)
        ((a b) 'not-equal))
      'equal)

(test "multiple clauses"
      (destructuring-match '(a b)
        ((x) 'one)
        ((x y) 'two)
        ((x y z) 'three))
      'two)

(test "dotted pattern"
      (destructuring-match '(1 2 3)
        ((a . b) (list a b)))
      '(1 (2 3)))

(test "underscore match"
      (destructuring-match '(1 2 3)
        ((_ _ a) a))
      3)

(test "complex list pattern"
      (destructuring-match '(let ((x 1) (y 2)) (+ x y))
        (('let bindings . body)
         (list bindings body)))
      '(((x 1) (y 2)) ((+ x y))))

(test "predicate match"
      (destructuring-match '(1 2)
        (((? fixnum? a) (? fixnum? b)) (list a b)))
      '(1 2))

(test "predicate match fail"
      (destructuring-match '("a" 2)
        (((? fixnum? a) (? fixnum? b)) 'match)
        (_ 'no-match))
      'no-match)

(test "tail ellipsis match"
      (destructuring-match '(1 2 3)
        ((a ... b) (list a b)))
      '((1 2) 3))

(test "n-tail ellipsis match"
      (destructuring-match '(1 2 3 4)
        ((a ... b c) (list a b c)))
      '((1 2) 3 4))

(test "no match returns #f"
      (destructuring-match '(1 2 3)
        ((a b) 'two))
      #f)

(test "empty list match"
      (destructuring-match '()
        (() 'empty)
        (_ 'not-empty))
      'empty)

(test "wildcard ellipsis"
      (destructuring-match '(1 2 3 4 5)
        ((_ ... a) a))
      5)

;; =============================================================================
;; Section 2: destructuring-bind
;; =============================================================================
(display "\n>>> Section 2: destructuring-bind\n")

(test "simple bind"
      (destructuring-bind (a b c) '(1 2 3) (+ a b c))
      6)

(test "nested bind"
      (destructuring-bind ((a b) (c d)) '((1 2) (3 4)) (list a b c d))
      '(1 2 3 4))

(test "dotted bind"
      (destructuring-bind (a . b) '(1 2 3) (list a b))
      '(1 (2 3)))

(test "underscore bind"
      (destructuring-bind (_ _ a) '(1 2 3) a)
      3)

;; =============================================================================
;; Section 3: pretty-print
;; =============================================================================
(display "\n>>> Section 3: pretty-print\n")

(define (pp-to-string expr)
  (let-values (((port extract) (open-string-output-port)))
    (pretty-print expr port)
    (extract)))

;; basic atoms
(test "pp atom number"
      (pp-to-string 42)
      "42\n")

(test "pp atom symbol"
      (pp-to-string 'hello)
      "hello\n")

(test "pp atom string"
      (pp-to-string "world")
      "\"world\"\n")

(test "pp atom boolean"
      (pp-to-string #t)
      "#t\n")

(test "pp empty list"
      (pp-to-string '())
      "()\n")

;; simple lists that fit on one line
(test "pp short list"
      (pp-to-string '(1 2 3))
      "(1 2 3)\n")

(test "pp nested list"
      (pp-to-string '(a (b c) d))
      "(a (b c) d)\n")

;; quote shorthands
(test "pp quote"
      (pp-to-string '(quote x))
      "'x\n")

(test "pp quasiquote"
      (pp-to-string '(quasiquote (a b)))
      "`(a b)\n")

;; vector
(test "pp empty vector"
      (pp-to-string '#())
      "#()\n")

(test "pp vector"
      (pp-to-string '#(1 2 3))
      "#(1 2 3)\n")

;; line wrapping with narrow width
(test "pp line wrapping"
      (parameterize ((pretty-print-line-length 20))
        (pp-to-string '(define (foo x y z) (+ x y z))))
      "(define (foo x y z)\n  (+ x y z))\n")

;; initial indent
(test "pp initial indent"
      (parameterize ((pretty-print-initial-indent 4)
                     (pretty-print-line-length 30))
        (pp-to-string '(a b c)))
      "(a b c)\n")

;; maximum lines
(test "pp maximum lines"
      (parameterize ((pretty-print-line-length 10)
                     (pretty-print-maximum-lines 2))
        (pp-to-string '(define (foo x y z w) (+ x y z w))))
      "(define (foo\n          x\n  ...\n")

;; special forms
(test "pp lambda"
      (parameterize ((pretty-print-line-length 15))
        (pp-to-string '(lambda (x) (+ x 1))))
      "(lambda (x)\n  (+ x 1))\n")

(test "pp if"
      (parameterize ((pretty-print-line-length 8))
        (pp-to-string '(if #t 1 2)))
      "(if #t\n    1\n    2)\n")

;; dotted pair
(test "pp dotted pair"
      (pp-to-string '(a . b))
      "(a . b)\n")

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
