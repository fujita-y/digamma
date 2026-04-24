;; Consolidated tests for test_core.scm

(define *pass-count* 0)
(define *fail-count* 0)

;; =============================================================================
;; test_r6rs_extra.scm
;; =============================================================================
;; test_r6rs_extra.scm
;; Test suite for extra R6RS features like variable transformers.



(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; =============================================================================
;; Section 1: Variable Transformers
;; =============================================================================
(display "\n>>> Section 1: Variable Transformers\n")

(macroexpand
 '(define-syntax var-trans
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
         ((set! _ val) (syntax (display (list 'setting val))))
         ((_ . rest) (syntax (list 1 . rest)))
         (_ (syntax 1)))))))

(test "var-trans-ref" (macroexpand 'var-trans 'strip) 1)
(test "var-trans-call" (macroexpand '(var-trans 2 3) 'strip) '(list 1 2 3))
(test "var-trans-set" (macroexpand '(set! var-trans 10) 'strip) '(display (list 'setting 10)))

;; =============================================================================
;; Section 2: identifier-syntax
;; =============================================================================
(display "\n>>> Section 2: identifier-syntax\n")

(macroexpand
 '(define-syntax id-test
    (identifier-syntax 42)))

(test "id-syntax-ref" (macroexpand 'id-test 'strip) 42)
(test "id-syntax-call" (macroexpand '(id-test 1 2) 'strip) '(42 1 2))

(macroexpand
 '(define-syntax p.car
    (identifier-syntax
     (p.car (car p))
     ((set! p.car val) (set-car! p val)))))

(test "id-syntax-set-ref" (macroexpand 'p.car 'strip) '(car p))
(test "id-syntax-set-set" (macroexpand '(set! p.car 99) 'strip) '(set-car! p 99))

(macroexpand
 '(define-syntax nested-id
    (identifier-syntax
     (nested-id (inner-macro))
     ((set! nested-id val) (set-inner! val)))))

(test "nested-id-set" (macroexpand '(set! nested-id 7) 'strip) '(set-inner! 7))

;; =============================================================================
;; Section 3: free-identifier=? tests
;; =============================================================================
(display "\n>>> Section 3: free-identifier=? tests\n")

(macroexpand
 '(define-syntax check-else
    (lambda (x)
      (syntax-case x (else)
        ((_ else) (syntax 'is-else))
        ((_ x) (syntax 'not-else))))))

(test "free-id-else-match" (macroexpand '(check-else else) 'strip) ''is-else)
(test "free-id-else-non-match" (macroexpand '(check-else other) 'strip) ''not-else)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)

;; =============================================================================
;; test_r7rs_extra.scm
;; =============================================================================
;; test_r7rs_extra.scm
;; Test suite for extra R7RS features in syntax-rules.



(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; =============================================================================
;; Section 1: R7RS syntax-rules Features
;; =============================================================================
(display "\n>>> Section 1: R7RS syntax-rules Features\n")

(macroexpand
 '(define-syntax nested-rules
    (syntax-rules ()
      ((_ (x ...))
       (let-syntax ((inner (syntax-rules dots ()
                             ((_ a dots) (list x ... a dots)))))
         (inner 1 2 3))))))

(test "nested-rules-ellipses" (macroexpand '(nested-rules (a b)) 'strip) '(list a b 1 2 3))

(macroexpand
 '(define-syntax lit-shadow
    (syntax-rules (=>)
      ((_ a => b) (list 'match a b))
      ((_ a b c) (list 'no-match a b c)))))

(test "lit-shadow-match" (macroexpand '(lit-shadow 1 => 2) 'strip) '(list 'match 1 2))

(test "lit-shadow-no-match"
      (macroexpand '(let ((=> #f)) (lit-shadow 1 => 2)) 'strip)
      '(let ((=> #f)) (list 'no-match 1 => 2)))

(macroexpand
 '(define-syntax improper-elli
    (syntax-rules ()
      ((_ (a ... . b)) (list (list a ...) b)))))

(test "improper-elli-match" (macroexpand '(improper-elli (1 2 3 . 4)) 'strip) '(list (list 1 2 3) 4))

(macroexpand
 '(define-syntax vec-nest
    (syntax-rules ()
      ((_ #((a b) ...)) (list (list a b) ...)))))

(test "vec-nest-match" (macroexpand '(vec-nest #((1 2) (3 4))) 'strip) '(list (list 1 2) (list 3 4)))

(macroexpand
 '(define-syntax custom-elli
    (syntax-rules ::: ()
      ((_ a :::) (list a :::)))))

(test "custom-elli-match" (macroexpand '(custom-elli 1 2 3) 'strip) '(list 1 2 3))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)

;; =============================================================================
;; test_quasiquote.scm
;; =============================================================================
;; test_quasiquote.scm
;; Test suite for quasiquote expansion in macroexpand system.


;; --- Test Helper Functions ---


(define (test name expr expected)
  (let ((result (macroexpand expr 'strip)))
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
;; Section 1: Quasiquote Basic Tests
;; =============================================================================
(display "\n>>> Section 1: Quasiquote Basic Tests\n")

(test "atom" '`a ''a)
(test "number" '`1 1)
(test "simple list" '`(1 2 3) '(list 1 2 3))
(test "simple unquote" '`(a ,2 c) '(list 'a 2 'c))
(test "unquote expression" '`(a ,(+ 1 1) c) '(list 'a (+ 1 1) 'c))

;; =============================================================================
;; Section 2: Quasiquote Splicing Tests
;; =============================================================================
(display "\n>>> Section 2: Quasiquote Splicing Tests\n")

(test "splicing only" '`(,@'(1 2)) ''(1 2))
(test "splicing start" '`(,@'(1 2) b) '(append '(1 2) (cons 'b ())))
(test "splicing end" '`(a ,@'(1 2)) '(cons 'a '(1 2)))
(test "splicing middle" '`(a ,@'(1 2) b) '(cons 'a (append '(1 2) (cons 'b ()))))
(test "multiple splicing" '`(,@'(1 2) ,@'(3 4)) '(append '(1 2) '(3 4)))

;; =============================================================================
;; Section 3: Quasiquote Dotted Pair Tests
;; =============================================================================
(display "\n>>> Section 3: Quasiquote Dotted Pair Tests\n")

(test "dotted pair no unquote" '`(a . b) '(cons 'a 'b))

;; =============================================================================
;; Section 4: Quasiquote Nested Tests
;; =============================================================================
(display "\n>>> Section 4: Quasiquote Nested Tests\n")

(test "nested level 1" '`(a `(b ,c d) e) '(list 'a (list 'quasiquote (list 'b (list 'unquote 'c) 'd)) 'e))

(test "nested level 2 (unquote-unquote)" '`(a `(b ,,c d) e) '(list 'a (list 'quasiquote (list 'b (list 'unquote c) 'd)) 'e))

(test "nested splicing level 1" '`(a `(b ,@c d) e) '(list 'a (list 'quasiquote (list 'b (list 'unquote-splicing 'c) 'd)) 'e))

(test "nested splicing level 2" '`(a `(b ,,@c d) e) '(list 'a (list 'quasiquote (list 'b (list 'unquote (list 'unquote-splicing 'c)) 'd)) 'e))

;; =============================================================================
;; Section 5: R7RS Examples
;; =============================================================================
(display "\n>>> Section 5: R7RS Examples\n")

(test "R7RS example 1" '`(list ,(+ 1 2) 4) '(list 'list (+ 1 2) 4))

(test "R7RS example 2" '(let ((name 'a)) `(list ,name ',name)) '(let ((name 'a)) (list 'list name (list 'quote name))))

(test "R7RS example 3" '`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) '(cons 'a (cons (+ 1 2) (append (map abs '(4 -5 6)) (cons 'b ())))))

(test "R7RS example 4 (dotted/splicing combo)" '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '(cons (list 'foo (- 10 3)) (append (cdr '(c)) (car '(cons)))))

;; =============================================================================
;; Section 6: Macro Definitions using Quasiquote
;; =============================================================================
(display "\n>>> Section 6: Macro Definitions using Quasiquote\n")

(macroexpand '(define-syntax test-qq (syntax-rules () ((_ x) `(result ,x)))))
(test "macro using quasiquote" '(test-qq 42) '(list 'result 42))

(macroexpand '(define-syntax test-splice (syntax-rules () ((_ x) `(item ,@x end)))))
(test "macro using unquote-splicing" '(test-splice x) '(cons 'item (append x (cons 'end ()))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)

;; =============================================================================
;; test_common.scm
;; =============================================================================
;; test_common.scm
;; Test suite for common.scm utilities.


(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; =============================================================================
;; Predicates
;; =============================================================================
(display "\n>>> Predicates\n")

(test "proper-list? (1 2 3)" (proper-list? '(1 2 3)) #t)
(test "proper-list? ()" (proper-list? '()) #t)
(test "proper-list? (1 . 2)" (proper-list? '(1 . 2)) #f)
(test "proper-list? 1" (proper-list? 1) #f)

#;(test "any (even? (1 2 3))" (any even? '(1 2 3)) #t)
#;(test "any (even? (1 3 5))" (any even? '(1 3 5)) #f)
#;(test "any (even? ())" (any even? '()) #f)

#;(test "every (even? (2 4 6))" (every even? '(2 4 6)) #t)
#;(test "every (even? (2 3 6))" (every even? '(2 3 6)) #f)
#;(test "every (even? ())" (every even? '()) #t)

;; =============================================================================
;; List Transformation & Iteration
;; =============================================================================
(display "\n>>> List Transformation\n")

#;(test "filter (even? (1 2 3 4))" (filter even? '(1 2 3 4)) '(2 4))
#;(test "filter (odd? (1 2 3 4))" (filter odd? '(1 2 3 4)) '(1 3))
#;(test "filter (even? ())" (filter even? '()) '())

(test "fold (+ 0 (1 2 3))" (fold + 0 '(1 2 3)) 6)
(test "fold (cons '() (1 2 3))" (fold cons '() '(1 2 3)) '(3 2 1))

(test "iota 3" (iota 3) '(0 1 2))
(test "iota 0" (iota 0) '())

#;(let-values (((in out) (partition even? '(1 2 3 4 5))))
  (test "partition (even? (1 2 3 4 5)) - in" in '(2 4))
  (test "partition (even? (1 2 3 4 5)) - out" out '(1 3 5)))

(test "list-head (1 2 3 4) 2" (list-head '(1 2 3 4) 2) '(1 2))
(test "list-head (1 2 3 4) 0" (list-head '(1 2 3 4) 0) '())
(test "list-head () 2" (list-head '() 2) '())

(test "list-tail (1 2 3 4) 2" (list-tail '(1 2 3 4) 2) '(3 4))
(test "list-tail (1 2 3 4) 0" (list-tail '(1 2 3 4) 0) '(1 2 3 4))
(test "list-tail () 2" (list-tail '() 2) '())

(test "string-join (\"a\" \"b\" \"c\") \",\"" (string-join '("a" "b" "c") ",") "a,b,c")
(test "string-join (\"a\") \",\"" (string-join '("a") ",") "a")
(test "string-join () \",\"" (string-join '() ",") "")

#;(test "map-improper (1 2 . 3)" (map-improper (lambda (x) (* x 2)) '(1 2 . 3)) '(2 4 . 6))
#;(test "map-improper (1 2 3)" (map-improper (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))

#;(test "flatten-begins ((begin 1 2) 3 (begin (begin 4 5) 6))" 
      (flatten-begins '((begin 1 2) 3 (begin (begin 4 5) 6)))
      '(1 2 3 4 5 6))

#;(test "remove-from-list (1 2 3 4 5) (2 4)" (remove-from-list '(1 2 3 4 5) '(2 4)) '(1 3 5))

#;(test "delete 2 (1 2 3 2 4)" (delete 2 '(1 2 3 2 4)) '(1 3 4))

;; =============================================================================
;; Patterns
;; =============================================================================
(display "\n>>> Patterns\n")

#;(test "match-rec-pattern (let ((f ...)) (set! f (lambda ...)))"
      (match-rec-pattern '(let ((f #f)) (set! f (lambda (x) x)) f))
      #t)

#;(test "match-rec-pattern (let ((f ...)) 123)"
      (match-rec-pattern '(let ((f #f)) 123))
      #f)

#;(test "match-rec-pattern simple tail recursion true match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (if (eq? n 0) 1 (<name> (- n 1))))) <name>))
      #t)

#;(test "match-rec-pattern tail recursion in let true match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (let ((x (- n 1))) (<name> x)))) <name>))
      #t)

#;(test "match-rec-pattern non-tail recursion false match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (list 2 (<name> 9) n))) <name>))
      #f)

#;(test "match-rec-pattern nested lambda false match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (if n (lambda () (<name> 9)) 1))) <name>))
      #f)

#;(test "generate-temporary-symbol test"
      (let ((s1 (generate-temporary-symbol "t"))
            (s2 (generate-temporary-symbol "t")))
        (and (symbol? s1) (symbol? s2) (not (eq? s1 s2))))
      #t)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)


(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin 
      (display "ALL TESTS PASSED.\n") 
      (exit 0))
    (begin
      (display "FAILED ")
      (display *fail-count*) 
      (display " TESTS.\n") 
      (exit 1)))
(newline)
