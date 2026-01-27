(load "./macroexpand.scm")

(define (assert-equal? name expected actual)
  (if (equal? expected actual)
      (begin (display "PASS: ") (display name) (newline))
      (begin (display "FAIL: ") (display name) (newline)
             (display "  Expected: ") (write expected) (newline)
             (display "  Actual:   ") (write actual) (newline))))

;; 1. Simple macro
;; Register foo using the expander's define-syntax handling
(expand '(define-syntax foo (syntax-rules ()
                              ((foo x) (list 'foo x)))))

(define test1 (macroexpand-1 '(foo 1)))
(display "Test 1 Expand: ") (write test1) (newline)
;; Now we expect expansion. 
;; (foo 1) -> (list 'foo 1). 
;; But syntax-rules introduces hygiene. 
;; The literal 'foo might be renamed or not? 
;; In this implementation, (list 'foo x) -> (list (quote foo) x).
;; 'foo is (quote foo). quote is core form.
;; Let's inspect the output first.

;; 2. Nested macro
(expand '(define-syntax bar (syntax-rules ()
                              ((bar x) (foo x)))))

(define test2 (macroexpand-1 '(bar 2)))
(display "Test 2 Expand: ") (write test2) (newline)
;; Should be (foo 2) expansion, which is... (list 'foo 2)?
;; NO. macroexpand-1 expands ONCE.
;; So (bar 2) -> (foo 2).
;; And (foo 2) is a macro call.

(define test2b (macroexpand-1 test2))
(display "Test 2b Expand: ") (write test2b) (newline)
;; Should be (list 'foo 2) (approximately).

;; 3. Non macro
(define test3 (macroexpand-1 '(list 1 2)))
(assert-equal? "Non-macro form" '(list 1 2) test3)

;; 4. Identifier macro
(register-macro! 'baz (lambda (expr) 'expanded-baz))
(define test4 (macroexpand-1 'baz))
(assert-equal? "Identifier macro" 'expanded-baz test4)


