;; test_quasiquote.scm
;; Test suite for quasiquote expansion in macroexpand system.

(load "./macroexpand.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test expected expr msg)
  (let ((expanded (macroexpand expr 'strip)))
    (if (equal? expected expanded)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ")
          (display msg)
          (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ")
          (display msg)
          (newline)
          (display "  Expected: ")
          (write expected)
          (newline)
          (display "  Actual:   ")
          (write expanded)
          (newline)))))

(display "\n>>> Quasiquote Basic Tests\n")

(test ''a '`a "atom")
(test 1 '`1 "number")
(test '(list 1 2 3) '`(1 2 3) "simple list")
(test '(list 'a 2 'c) '`(a ,2 c) "simple unquote")
(test '(list 'a (+ 1 1) 'c) '`(a ,(+ 1 1) c) "unquote expression")

(display "\n>>> Quasiquote Splicing Tests\n")

(test ''(1 2) '`(,@'(1 2)) "splicing only")
(test '(append '(1 2) (cons 'b ())) '`(,@'(1 2) b) "splicing start")
(test '(cons 'a '(1 2)) '`(a ,@'(1 2)) "splicing end")
(test '(cons 'a (append '(1 2) (cons 'b ()))) '`(a ,@'(1 2) b) "splicing middle")
(test '(append '(1 2) '(3 4)) '`(,@'(1 2) ,@'(3 4)) "multiple splicing")

(display "\n>>> Quasiquote Dotted Pair Tests\n")

(test '(cons 'a 'b) '`(a . b) "dotted pair no unquote")

(display "\n>>> Quasiquote Nested Tests\n")

(test '(list 'a (list 'quasiquote (list 'b (list 'unquote 'c) 'd)) 'e)
      '`(a `(b ,c d) e)
      "nested level 1")

(test '(list 'a (list 'quasiquote (list 'b (list 'unquote c) 'd)) 'e)
      '`(a `(b ,,c d) e)
      "nested level 2 (unquote-unquote)")

(test '(list 'a (list 'quasiquote (list 'b (list 'unquote-splicing 'c) 'd)) 'e)
      '`(a `(b ,@c d) e)
      "nested splicing level 1")

(test '(list 'a (list 'quasiquote (list 'b (list 'unquote (list 'unquote-splicing 'c)) 'd)) 'e)
      '`(a `(b ,,@c d) e)
      "nested splicing level 2")

(display "\n>>> R7RS Examples\n")

(test '(list 'list (+ 1 2) 4)
      '`(list ,(+ 1 2) 4)
      "R7RS example 1")

(test '(let ((name 'a)) (list 'list name (list 'quote name)))
      '(let ((name 'a)) `(list ,name ',name))
      "R7RS example 2")

(test '(cons 'a (cons (+ 1 2) (append (map abs '(4 -5 6)) (cons 'b ()))))
      '`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
      "R7RS example 3")

(test '(cons (list 'foo (- 10 3)) (append (cdr '(c)) (car '(cons))))
      '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
      "R7RS example 4 (dotted/splicing combo)")

(display "\n>>> Macro Definitions using Quasiquote\n")

(macroexpand '(define-syntax test-qq (syntax-rules () ((_ x) `(result ,x)))))
(test '(list 'result 42)
      '(test-qq 42)
      "macro using quasiquote")

(macroexpand '(define-syntax test-splice (syntax-rules () ((_ x) `(item ,@x end)))))
(test '(cons 'item (append x (cons 'end ())))
      '(test-splice x)
      "macro using unquote-splicing")

;; --- Summary ---

(newline)
(if (= *fail-count* 0)
    (display "ALL QUASIQUOTE TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
