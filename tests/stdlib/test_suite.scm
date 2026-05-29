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

;; -------------------------------------
;; (core let-values)
;; -------------------------------------
(test-begin "core let-values")

(test-eval! (import (core let-values)))

(test-equal "let-values single binding" 3
  (let-values (((x y) (values 1 2)))
    (+ x y)))

(test-equal "let-values one-element values" 42
  (let-values (((x) (values 42)))
    x))

(test-equal "let-values multiple bindings" (1 2 3 4)
  (let-values (((x y) (values 1 2))
               ((a b) (values 3 4)))
    (list x y a b)))

(test-equal "let*-values sequential bindings" 36
  (let*-values (((x y) (values 1 2))
                ((z w) (values (+ x 10) (+ y 20))))
    (+ x y z w)))

(test-end)

;; -------------------------------------
;; (core parameterize)
;; -------------------------------------
(test-begin "core parameterize")

(test-eval! (import (core parameterize)))

(test-equal "parameterize simple binding" 2
  (let ((p (make-parameter 1)))
    (parameterize ((p 2))
      (p))))

(test-equal "parameterize multiple bindings" (2 3)
  (let ((p (make-parameter 1))
        (q (make-parameter 2)))
    (parameterize ((p 2) (q 3))
      (list (p) (q)))))

(test-equal "parameterize restores original values" (1 2)
  (let ((p (make-parameter 1))
        (q (make-parameter 2)))
    (parameterize ((p 3) (q 4))
      (list (p) (q)))
    (list (p) (q))))

(test-end)

;; -------------------------------------
;; (core struct)
;; -------------------------------------
(test-begin "core struct")

(test-eval! (import (core struct)))

(test-eval!
 (define-struct test-struct (x y)))

(test-equal "struct predicate from core struct" #t
  (let ((p (make-test-struct 1 2)))
    (test-struct? p)))

(test-equal "struct accessors from core struct" (1 2)
  (let ((p (make-test-struct 1 2)))
    (list (test-struct-x p) (test-struct-y p))))

(test-equal "struct setters from core struct" (3 4)
  (let ((p (make-test-struct 1 2)))
    (test-struct-x-set! p 3)
    (test-struct-y-set! p 4)
    (list (test-struct-x p) (test-struct-y p))))

(test-end)

;; -------------------------------------
;; (core records)
;; -------------------------------------
(test-begin "r6rs records")

(test-eval! (import (core records)))

;; 1. Basic define-record-type (auto-named constructor/predicate)
(test-eval!
 (define-record-type point
   (fields (immutable x)
           (immutable y))))

(test-equal "auto-named constructor produces record" #t
  (point? (make-point 3 4)))

(test-eqv "auto-named accessor x" 3
  (point-x (make-point 3 4)))

(test-eqv "auto-named accessor y" 4
  (point-y (make-point 3 4)))

(test-equal "predicate rejects non-record" #f
  (point? #f))

;; 2. define-record-type with explicit names
(test-eval!
 (define-record-type (rect make-rect rect?)
   (fields (immutable width  rect-width)
           (immutable height rect-height))))

(test-equal "explicit constructor/predicate" #t
  (rect? (make-rect 10 20)))

(test-eqv "explicit accessor width" 10
  (rect-width (make-rect 10 20)))

(test-eqv "explicit accessor height" 20
  (rect-height (make-rect 10 20)))

;; 3. Mutable fields and mutation
(test-eval!
 (define-record-type (counter make-counter counter?)
   (fields (mutable value counter-value counter-value-set!))))

(test-eval! (define c (make-counter 0)))

(test-eqv "mutable field initial value" 0
  (counter-value c))

(test-eval! (counter-value-set! c 42))

(test-eqv "mutable field after set!" 42
  (counter-value c))

;; 4. Mixed immutable and mutable fields
(test-eval!
 (define-record-type (person make-person person?)
   (fields (immutable name   person-name)
           (mutable   age    person-age person-age-set!))))

(test-eval! (define p (make-person "Alice" 30)))

(test-equal "mixed fields: immutable name" "Alice"
  (person-name p))

(test-eqv "mixed fields: mutable age" 30
  (person-age p))

(test-eval! (person-age-set! p 31))

(test-eqv "mixed fields: age after mutation" 31
  (person-age p))

;; 5. Inheritance (parent clause)
(test-eval!
 (define-record-type (colored-point make-colored-point colored-point?)
   (parent point)
   (fields (immutable color colored-point-color))))

(test-eval! (define cp (make-colored-point 1 2 'red)))

(test-equal "child predicate" #t
  (colored-point? cp))

(test-equal "parent predicate accepts child" #t
  (point? cp))

(test-eqv "inherited accessor x" 1
  (point-x cp))

(test-eqv "inherited accessor y" 2
  (point-y cp))

(test-eq "child accessor color" (colored-point-color cp) => red)

;; 6. Protocol customization
(test-eval!
 (define-record-type (interval make-interval interval?)
   (fields (immutable lo interval-lo)
           (immutable hi interval-hi))
   (protocol
    (lambda (new)
      (lambda (lo hi)
        (if (> lo hi)
            (new hi lo)   ; swap so lo <= hi always
            (new lo hi)))))))

(test-eqv "protocol swaps lo/hi when needed - lo" 2
  (interval-lo (make-interval 5 2)))

(test-eqv "protocol swaps lo/hi when needed - hi" 5
  (interval-hi (make-interval 5 2)))

(test-eqv "protocol preserves order - lo" 1
  (interval-lo (make-interval 1 9)))

;; 7. RTD introspection
(test-equal "record-type-name" (record-type-name (record-type-descriptor point)) => point)

(test-equal "record-type-parent is #f for root" #f
  (record-type-parent (record-type-descriptor point)))

(test-equal "record-type-parent names child's parent" (record-type-name (record-type-parent (record-type-descriptor colored-point))) => point)

(test-equal "record-type-field-names" #(x y)
  (record-type-field-names (record-type-descriptor point)))

(test-equal "record-field-mutable? immutable" #f
  (record-field-mutable? (record-type-descriptor point) 0))

(test-equal "record-field-mutable? mutable" #t
  (record-field-mutable? (record-type-descriptor counter) 0))

(test-equal "record-type-sealed? default #f" #f
  (record-type-sealed? (record-type-descriptor point)))

(test-equal "record-type-opaque? default #f" #f
  (record-type-opaque? (record-type-descriptor point)))

(test-equal "record-type-generative? returns #t for generative" #t
  (record-type-generative? (record-type-descriptor point)))

;; 8. Procedural layer
(test-eval!
 (define vec2-rtd
   (make-record-type-descriptor 'vec2 #f #f #f #f
     '#((immutable dx) (immutable dy)))))

(test-eval!
 (define vec2-rcd
   (make-record-constructor-descriptor vec2-rtd #f #f)))

(test-eval! (define make-vec2   (record-constructor   vec2-rcd)))
(test-eval! (define vec2?       (record-predicate     vec2-rtd)))
(test-eval! (define vec2-dx     (record-accessor      vec2-rtd 0)))
(test-eval! (define vec2-dy     (record-accessor      vec2-rtd 1)))

(test-eval! (define v (make-vec2 3 4)))

(test-equal "procedural: predicate" #t
  (vec2? v))

(test-eqv "procedural: accessor dx" 3
  (vec2-dx v))

(test-eqv "procedural: accessor dy" 4
  (vec2-dy v))

;; Mutable field via procedural layer
(test-eval!
 (define box-rtd
   (make-record-type-descriptor 'box #f #f #f #f
     '#((mutable val)))))

(test-eval!
 (define box-rcd
   (make-record-constructor-descriptor box-rtd #f #f)))

(test-eval! (define make-box  (record-constructor  box-rcd)))
(test-eval! (define box?      (record-predicate    box-rtd)))
(test-eval! (define box-val   (record-accessor     box-rtd 0)))
(test-eval! (define box-val-set! (record-mutator   box-rtd 0)))

(test-eval! (define b (make-box 99)))

(test-eqv "procedural mutable: initial" 99
  (box-val b))

(test-eval! (box-val-set! b 7))

(test-eqv "procedural mutable: after mutator" 7
  (box-val b))

;; 9. record? and record-rtd
(test-equal "record? on a record" #t
  (record? (make-point 0 0)))

(test-equal "record? on non-record" #f
  (record? #f))

(test-equal "record-rtd returns correct rtd name" (record-type-name (record-rtd (make-point 0 0))) => point)

;; 10. Sealed record type
(test-eval!
 (define-record-type (sealed-rec make-sealed-rec sealed-rec?)
   (fields (immutable val sealed-rec-val))
   (sealed #t)))

(test-equal "sealed: record-type-sealed?" #t
  (record-type-sealed? (record-type-descriptor sealed-rec)))

;; 11. Opaque record type
(test-eval!
 (define-record-type (opaque-rec make-opaque-rec opaque-rec?)
   (fields (immutable secret opaque-rec-secret))
   (opaque #t)))

(test-eval! (define or-instance (make-opaque-rec 42)))

(test-equal "opaque: record? returns #f" #f
  (record? or-instance))

;; 12. Nongenerative record type (same uid returns same RTD)
(test-eval!
 (define-record-type (tagged make-tagged tagged?)
   (fields (immutable tag tagged-tag))
   (nongenerative my-lib:tagged)))

(test-eval!
 (define tagged-rtd-1 (record-type-descriptor tagged)))

;; Re-registering with same uid must yield the identical RTD
(test-eval!
 (define-record-type (tagged make-tagged tagged?)
   (fields (immutable tag tagged-tag))
   (nongenerative my-lib:tagged)))

(test-eval!
 (define tagged-rtd-2 (record-type-descriptor tagged)))

(test-equal "nongenerative: same uid => same RTD object" #t
  (eq? tagged-rtd-1 tagged-rtd-2))

(test-end)

;; -------------------------------------
;; (srfi :197)
;; -------------------------------------
(test-begin "srfi-197: pipeline operators")

(test-eval! (import (srfi :197)))
(test-eval! (define (exclamation x) (string-append x "!")))
(test-eval! (define (foo+bar x) (values (string-append x "foo") (string-append x "bar"))))

(test-equal "chain" "bazbarfoo!"
  (chain ""
         (string-append "foo" _)
         (string-append "bar" _)
         (string-append "baz" _)
         (exclamation _)))

(test-equal "chain with mixed _ position" "barfoobaz"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" _)
         (string-append _ "baz")))

(test-equal "chain with _ in operator position" 3
  (chain +
         (_ 1 2)))

(test-equal "chain without _" "barbazqux"
  (chain ""
         (string-append _ "foo")
         (string-append "bar" "baz")
         (string-append _ "qux")))

(test-equal "chain multiple _" "quxfoo/quxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "/" _)))

(test-equal "chain _ ..." "bazquxfooquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append "baz" _ ...)))

(test-equal "chain _ _ ..." "quxfoobazquxbar"
  (chain "qux"
         (foo+bar _)
         (string-append _ "baz" _ ...)))

(test-equal "chain with custom _" "bazbarfoo!"
  (chain "" <>
         (string-append "foo" <>)
         (string-append "bar" <>)
         (string-append "baz" <>)
         (exclamation <>)))

(test-equal "chain with custom ..." "bazquxfooquxbar"
  (chain "qux" - ---
         (foo+bar -)
         (string-append "baz" - ---)))

(test-equal "chain-and" "bazbarfoo!"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" _)
             (string-append "baz" _)
             (exclamation _)))

(test-equal "chain-and with mixed _ position" "barfoobaz"
  (chain-and ""
             (string-append _ "foo")
             (string-append "bar" _)
             (string-append _ "baz")))

(test-equal "chain-and without _" "barbazqux"
  (chain-and ""
             (string-append "foo" _)
             (string-append "bar" "baz")
             (string-append _ "qux")))

(test-equal "chain-and short-circuit" #f
  (chain-and ""
             (string-append "foo" _)
             (equal? _ "bar")
             (string-append "baz" _)
             (exclamation _)))

(test-equal "chain-and short-circuit first" #f
  (chain-and #f
             (not _)))

(test-equal "chain-and with custom _" "bazbarfoo!"
  (chain-and "" <>
             (string-append "foo" <>)
             (string-append "bar" <>)
             (string-append "baz" <>)
             (exclamation <>)))

(test-equal "chain-when" "bazfoo"
  (chain-when ""
              ((= (+ 2 2) 4) (string-append "foo" _))
              ((= (+ 2 2) 5) (string-append "bar" _))
              (#t (string-append "baz" _))))

(test-equal "chain-when with mixed _ position" "barfooqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar" _))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test-equal "chain-when without _" "barqux"
  (chain-when ""
              (#t (string-append _ "foo"))
              (#t (string-append "bar"))
              (#f (string-append _ "baz"))
              (#t (string-append _ "qux"))))

(test-equal "chain-when with custom _" "bazfoo"
  (chain-when "" <>
              ((= (+ 2 2) 4) (string-append "foo" <>))
              ((= (+ 2 2) 5) (string-append "bar" <>))
              (#t (string-append "baz" <>))))

(test-equal "chain-lambda" "bazbarfoo!"
  ((chain-lambda (string-append "foo" _)
                 (string-append "bar" _)
                 (string-append "baz" _)
                 (exclamation _))
   ""))

(test-equal "chain-lambda one step" "foobar"
  ((chain-lambda (string-append "foo" _)) "bar"))

(test-equal "chain-lambda with mixed _ position" "barfoobaz"
  ((chain-lambda (string-append _ "foo")
                 (string-append "bar" _)
                 (string-append _ "baz"))
   ""))

(test-equal "chain-lambda multiple _" "foobarbazqux"
  ((chain-lambda (string-append _ "bar" _)
                 (string-append _ "qux"))
   "foo"
   "baz"))

(test-equal "chain-lambda without _" "barqux"
  ((chain-lambda (string-append "bar")
                 (string-append _ "qux"))))

(test-equal "chain-lambda _ ..." "foobarbazqux"
  ((chain-lambda (string-append "foo" _ ...)
                 (string-append _ "qux"))
   "bar"
   "baz"))

(test-equal "chain-lambda _ _ ..." "foobarbazquxquux"
  ((chain-lambda (string-append _ "bar" _ ...)
                 (string-append _ "quux"))
   "foo"
   "baz"
   "qux"))

(test-equal "chain-lambda with custom _" "bazbarfoo!"
  ((chain-lambda <>
                 (string-append "foo" <>)
                 (string-append "bar" <>)
                 (string-append "baz" <>)
                 (exclamation <>))
   ""))

(test-equal "chain-lambda with custom ..." "foobarbazqux"
  ((chain-lambda - ---
                 (string-append "foo" - ---)
                 (string-append - "qux"))
   "bar"
   "baz"))

(test-equal "nest" (1 2 (3 (4) 5))
  (nest (quote _)
        (1 2 _)
        (3 _ 5)
        (_)
        4))

(test-equal "nest with custom _" (1 2 (3 (4) 5))
  (nest <>
        (quote <>)
        (1 2 <>)
        (3 <> 5)
        (<>)
        4))

(test-equal "nested nest" (1 2 3 (4 5 6))
  (nest (nest _2 (quote _2) (1 2 3 _2) _ 6)
        (_ 5 _2)
        4))

(test-equal "nest-reverse" (1 2 (3 (4) 5))
  (nest-reverse 4
                (_)
                (3 _ 5)
                (1 2 _)
                (quote _)))

(test-equal "nest-reverse with custom _" (1 2 (3 (4) 5))
  (nest-reverse 4 <>
                (<>)
                (3 <> 5)
                (1 2 <>)
                (quote <>)))

(test-end)

;;;

(test-report)
(exit)