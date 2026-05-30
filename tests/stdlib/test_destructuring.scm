(import (core test-lite))

;; -------------------------------------
;; (core destructuring)
;; -------------------------------------
(test-begin "core destructuring")

(test-eval! (import (core destructuring)))

(test-equal "destructuring-match simple match" 6
  (destructuring-match '(1 2 3) ((a b c) (+ a b c))))

(test-equal "destructuring-match quote match" (1)
  (destructuring-match '(quote 1) (('quote e) (list 1)) (_ (list 'nomatch))))

(test-equal "destructuring-match ellipsis match" (1 (2 3 4) 5)
  (destructuring-match '(1 2 3 4 5) ((a b ... c) (list a b c))))

(test-equal "destructuring-match nested lambda call" ((x y) ((+ x y)) (1 2))
  (destructuring-match '((lambda (x y) (+ x y)) 1 2)
    ((('lambda (vars ...) . body) . args)
     (and (= (length vars) (length args))
          (list vars body args)))))

(test-equal "destructuring-match boolean literal #t" 1
  (destructuring-match '(#t 1 2)
    ((#t e1 . _) e1)))

(test-equal "destructuring-match boolean literal #f" (2)
  (destructuring-match '(#f 1 2)
    ((#f _ . e2) e2)))

(test-equal "destructuring-match literal symbol" x
  (destructuring-match '(not x)
    (('not e1) e1)))

(test-equal "destructuring-match fender order" not-equal
  (destructuring-match '(1 2)
    ((a b) (= a b) 'equal)
    ((a b) 'not-equal)))

(test-equal "destructuring-match fender success" equal
  (destructuring-match '(1 1)
    ((a b) (= a b) 'equal)
    ((a b) 'not-equal)))

(test-equal "destructuring-match multiple clauses" two
  (destructuring-match '(a b)
    ((x) 'one)
    ((x y) 'two)
    ((x y z) 'three)))

(test-equal "destructuring-match dotted pattern" (1 (2 3))
  (destructuring-match '(1 2 3)
    ((a . b) (list a b))))

(test-equal "destructuring-match underscore wildcard" 3
  (destructuring-match '(1 2 3)
    ((_ _ a) a)))

(test-equal "destructuring-match complex list pattern" (((x 1) (y 2)) ((+ x y)))
  (destructuring-match '(let ((x 1) (y 2)) (+ x y))
    (('let bindings . body)
     (list bindings body))))

(test-equal "destructuring-match predicate match" (1 2)
  (destructuring-match '(1 2)
    (((? fixnum? a) (? fixnum? b)) (list a b))))

(test-equal "destructuring-match predicate fail" no-match
  (destructuring-match '("a" 2)
    (((? fixnum? a) (? fixnum? b)) 'match)
    (_ 'no-match)))

(test-equal "destructuring-match tail ellipsis match" ((1 2) 3)
  (destructuring-match '(1 2 3)
    ((a ... b) (list a b))))

(test-equal "destructuring-match n-tail ellipsis match" ((1 2) 3 4)
  (destructuring-match '(1 2 3 4)
    ((a ... b c) (list a b c))))

(test-equal "destructuring-match no match returns #f" #f
  (destructuring-match '(1 2 3)
    ((a b) 'two)))

(test-equal "destructuring-match empty list match" empty
  (destructuring-match '()
    (() 'empty)
    (_ 'not-empty)))

(test-equal "destructuring-match wildcard ellipsis" 5
  (destructuring-match '(1 2 3 4 5)
    ((_ ... a) a)))

(test-equal "destructuring-bind simple bind" 6
  (destructuring-bind (a b c) '(1 2 3) (+ a b c)))

(test-equal "destructuring-bind nested bind" (1 2 3 4)
  (destructuring-bind ((a b) (c d)) '((1 2) (3 4))
    (list a b c d)))

(test-equal "destructuring-bind dotted bind" (1 (2 3))
  (destructuring-bind (a . b) '(1 2 3)
    (list a b)))

(test-equal "destructuring-bind underscore bind" 3
  (destructuring-bind (_ _ a) '(1 2 3) a))

(test-equal "destructuring-bind ellipsis bind" (1 2 3)
  (destructuring-bind (a ...) '(1 2 3) a))

(test-end)

(test-report)
(exit)
