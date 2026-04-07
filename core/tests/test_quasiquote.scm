;; test_quasiquote.scm
;; Test suite for quasiquote expansion in macroexpand system.


;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

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
