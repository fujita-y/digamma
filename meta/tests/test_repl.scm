;;; Test for REPL logic
(load "../../core/core.scm")
(load "../repl.scm")

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (format #t "Testing: ~a\n" name)
  (let* ((expanded (macroexpand expr))
         (optimized (optimize expanded))
         (code (generate-bytecode (compile optimized)))
         (ctx (vm:init-context code)))
    (repl:init-globals)
    (repl:eval-prelude)
    (let ((result (vm:vm-run ctx)))
      (if (equal? result expected)
          (begin
            (set! *pass-count* (+ *pass-count* 1))
            (format #t "  PASS: ~s\n" result))
          (begin
            (set! *fail-count* (+ *fail-count* 1))
            (format #t "  FAIL: expected ~s, got ~s\n" expected result)
            (exit 1))))))

;; =============================================================================
(display "\n>>> Section 1: Basic Integration & Core Forms\n")
;; =============================================================================

(test "prelude: map" '(map (lambda (x) (+ x 1)) '(1 2 3)) '(2 3 4))
(test "simple addition" '(+ 1 2) 3)
(test "let-binding" '(let ((x 10)) (+ x 20)) 30)
(test "if-expression" '(if (= 0 0) 'yes 'no) 'yes)
(test "lambda application" '((lambda (x) (* x x)) 5) 25)
(test "lambda internal define" '((lambda (x) (define y 1) (+ x y)) 10) 11)

;; =============================================================================
(display "\n>>> Section 2: Macro (syntax-rules)\n")
;; =============================================================================

(test "define-syntax" '(define-syntax foo (syntax-rules () ((foo x) (+ x 1)))) 'defined)
(test "let-syntax" '(let-syntax ((foo (syntax-rules () ((foo x) (+ x 1))))) (foo 10)) 11)

(test "my-or macro"
      '(begin
         (define-syntax my-or
           (syntax-rules ()
             ((_ a b)
              (let ((t a))
                (if t t b)))))
         (my-or #t #f))
      #t)

;; =============================================================================
(display "\n>>> Section 3: Macro (syntax-case)\n")
;; =============================================================================

(test "reverse-params"
      '(begin
         (define-syntax reverse-params
           (lambda (x)
             (syntax-case x ()
               ((_ a b c) (syntax (list c b a))))))
         (reverse-params 1 2 3))
      '(3 2 1))

(test "r6rs-or (recursion)"
      '(begin
         (define-syntax r6rs-or
           (lambda (x)
             (syntax-case x ()
               [(_) (syntax #f)]
               [(_ e) (syntax e)]
               [(_ e1 e2 e3 ...)
                (syntax (let ([t e1])
                          (if t t (r6rs-or e2 e3 ...))))])))
         (r6rs-or 1 2 3))
      1)

(test "anaphoric if (aif)"
      '(begin
         (define-syntax aif
           (lambda (x)
             (syntax-case x ()
               ((_ test true false)
                (with-syntax ((it (datum->syntax (syntax test) 'it)))
                  (syntax (let ((it test))
                            (if it true false))))))))
         (aif (assoc 'a '((a . 1))) (cdr it) #f))
      1)

(test "quasisyntax template"
      '(begin
         (define-syntax test-quasisyntax
           (lambda (x)
             (syntax-case x ()
               ((_ val)
                (quasisyntax (list (unsyntax (syntax val)) (unsyntax (+ (syntax->datum (syntax val)) 1))))))))
         (test-quasisyntax 10))
      '(10 11))

(test "define-struct"
      '(begin
         (define-syntax define-struct
           (lambda (x)
             (syntax-case x ()
               ((_ name (field ...))
                (with-syntax ((make-name (datum->syntax (syntax name)
                                                        (string->symbol
                                                         (string-append "make-"
                                                                        (symbol->string
                                                                         (syntax->datum (syntax name)))))))
                              (name? (datum->syntax (syntax name)
                                                    (string->symbol
                                                     (string-append (symbol->string
                                                                     (syntax->datum (syntax name))) "?")))))
                  (syntax (begin
                            (define (make-name field ...) (list (quote name) field ...))
                            (define (name? obj) (and (pair? obj) (eq? (car obj) (quote name)))))))))))
         (define-struct point (x y))
         (list (point? (make-point 1 2)) (make-point 1 2)))
      '(#t (point 1 2)))

(test "generate-temporaries"
      '(begin
         (define-syntax test-gen-temp
           (lambda (x)
             (syntax-case x ()
               ((_ x ...)
                (let ((temps (generate-temporaries (syntax (x ...)))))
                  (with-syntax (((t ...) temps))
                    (syntax (let ((t x) ...) (list t ...)))))))))
         (test-gen-temp 1 2 3))
      '(1 2 3))

;; =============================================================================
(display "\n>>> Section 4: Hygiene & Scoping\n")
;; =============================================================================

(test "hygienic swap"
      '(begin
         (define-syntax swap
           (syntax-rules ()
             ((_ a b)
              (let ((temp a))
                (set! a b)
                (set! b temp)))))
         (let ((temp 1) (other 2))
           (swap temp other)
           temp))
      2)

(test "let-syntax shadowing"
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (let-syntax ((m (syntax-rules () ((m x) (+ 2 2)))))
           (m 1)))
      4)

(test "operator shadowing"
      '(let-syntax ((foo
                    (syntax-rules ()
                      ((_ expr) (+ expr 1)))))
         (let ((+ *))
           (foo 3)))
      4)

(test "nested macro hygiene (blue/red)"
      '(begin
         (define-syntax blue
           (syntax-rules ()
             ((blue x)
              (let-syntax ((red (syntax-rules ()
                                  ((red y) (list x y)))))
                (red 'z)))))
         (let ((list (lambda (x y) 'captured)))
           (blue 'w)))
      '(w z))

(test "local macro capture definition site"
      '(let ((x 1))
         (let-syntax ((get-x (syntax-rules () ((_) x))))
           (let ((x 2))
             (get-x))))
      1)

(test "shadowing core lambda with macro"
      '(begin
         (define-syntax my-lambda
           (syntax-rules ()
             ((_ (v) body) (lambda (v) body))))
         (let-syntax ((lambda (syntax-rules () ((_ args body) 'captured))))
           ((my-lambda (y) y) 42)))
      42)

;; =============================================================================
(display "\n>>> Section 5: Quasiquote Tests\n")
;; =============================================================================

(test "quasiquote: atom" '`a 'a)
(test "quasiquote: simple list" '`(1 2 3) '(1 2 3))
(test "quasiquote: simple unquote" '`(a ,2 c) '(a 2 c))
(test "quasiquote: unquote expression" '`(a ,(+ 1 1) c) '(a 2 c))
(test "quasiquote: splicing end" '`(a ,@'(1 2)) '(a 1 2))
(test "quasiquote: multiple splicing" '`(,@'(1 2) ,@'(3 4)) '(1 2 3 4))
(test "quasiquote: R7RS combo" '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))

;; =============================================================================
(display "\n>>> Section 6: Standard Extensions (R6RS/R7RS)\n")
;; =============================================================================

(test "R6RS: variable transformer"
      '(begin
         (define-syntax var-trans
           (make-variable-transformer
            (lambda (x)
              (syntax-case x (set!)
                ((set! _ val) (list 'display (list 'quote (list 'setting val))))
                ((_ . rest) (syntax (list 1 . rest)))
                (_ (syntax 1))))))
         (list var-trans (var-trans 2 3)))
      '(1 (1 2 3)))

(test "R6RS: identifier-syntax"
      '(begin
         (define-syntax id-test
           (identifier-syntax 42))
         id-test)
      42)

(test "R6RS: identifier-syntax (p.car)"
      '(begin
         (define-syntax p.car
           (lambda (x)
             (syntax-case x ()
               [(_ . rest) (syntax ((car p) . rest))]
               [_ (syntax (car p))])))
         (define p (list + 1 2 3))
         (p.car 4 5))
      9)

(test "R7RS: nested rules with custom ellipses"
      '(begin
         (define-syntax nested-rules
           (syntax-rules ()
             ((_ (x ...))
              (let-syntax ((inner (syntax-rules dots ()
                                    ((_ a dots) (list x ... a dots)))))
                (inner 1 2 3)))))
         (nested-rules (+ -)))
      (list + - 1 2 3))

(test "R7RS: improper list in syntax-rules"
      '(begin
         (define-syntax improper-elli
           (syntax-rules ()
             ((_ (a ... . b)) (list (list a ...) b))))
         (improper-elli (1 2 3 . 4)))
      '((1 2 3) 4))

(test "R7RS: vector patterns in syntax-rules"
      '(begin
         (define-syntax vec-nest
           (syntax-rules ()
             ((_ #((a b) ...)) (list (list a b) ...))))
         (vec-nest #((1 2) (3 4))))
      '((1 2) (3 4)))

;; =============================================================================
(display "\n>>> Section 7: TSPL Tests\n")
;; =============================================================================

(test "tspl: reciprocal"
      '(begin
         (define reciprocal
           (lambda (n)
             (if (= n 0) "oops!" (/ 1 n))))
         (list (reciprocal 10) (reciprocal 0)))
      '(1/10 "oops!"))

(test "tspl: square"
      '(begin
         (define square (lambda (n) (* n n)))
         (list (square 5) (square -2)))
      '(25 4))

(test "tspl: nested let"
      '(let ((a 4) (b -3))
         (let ((a-squared (* a a))
               (b-squared (* b b)))
           (+ a-squared b-squared)))
      25)

(test "tspl: higher-order doubler"
      '(begin
         (define doubler
           (lambda (f)
             (lambda (x) (f x x))))
         (define double (doubler +))
         (double 13/2))
      13)

(test "tspl: complex let"
      '(let ((list1 '(a b c)) (list2 '(d e f)))
         (cons (cons (car list1) (car list2))
               (cons (car (cdr list1)) (car (cdr list2)))))
      '((a . d) b . e))

(test "tspl: income-tax (cond)"
      '(begin
         (define income-tax
           (lambda (income)
             (cond
              ((<= income 10000) (* income 0.05))
              ((<= income 20000) (+ (* (- income 10000) 0.08) 500.0))
              ((<= income 30000) (+ (* (- income 20000) 0.13) 1300.0))
              (else (+ (* (- income 30000) 0.21) 2600.0)))))
         (list (income-tax 5000) (income-tax 15000) (income-tax 25000) (income-tax 50000)))
      '(250.0 900.0 1950.0 6800.0))

(test "tspl: recursive remv"
      '(begin
         (define proc-remv
           (lambda (x ls)
             (cond
              ((null? ls) '())
              ((eqv? (car ls) x) (proc-remv x (cdr ls)))
              (else (cons (car ls) (proc-remv x (cdr ls)))))))
         (list (proc-remv 'a '(a b b d)) (proc-remv 'b '(a b b d))))
      '((b b d) (a d)))

(test "tspl: recursive length"
      '(begin
         (define proc-length
           (lambda (ls)
             (if (null? ls)
                 0
                 (+ (proc-length (cdr ls)) 1))))
         (proc-length '(a b c)))
      3)

(test "tspl: recursive memv"
      '(begin
         (define proc-memv
           (lambda (x ls)
             (cond
              ((null? ls) #f)
              ((eqv? (car ls) x) ls)
              (else (proc-memv x (cdr ls))))))
         (proc-memv 'b '(a b b d)))
      '(b b d))

(test "tspl: tree-copy"
      '(begin
         (define proc-tree-copy
           (lambda (tr)
             (if (not (pair? tr))
                 tr
                 (cons (proc-tree-copy (car tr))
                       (proc-tree-copy (cdr tr))))))
         (proc-tree-copy '((a . b) . c)))
      '((a . b) . c))

(test "tspl: quadratic-formula"
      '(begin
         (define quadratic-formula
           (lambda (a b c)
             (let ((minusb (- 0 b))
                   (radical (sqrt (- (* b b) (* 4 (* a c)))))
                   (divisor (* 2 a)))
               (let ((root1 (/ (+ minusb radical) divisor))
                     (root2 (/ (- minusb radical) divisor)))
                 (cons root1 root2)))))
         (quadratic-formula 2 -4 -6))
      '(3 . -1))

;; =============================================================================
(display "\n>>> Section 8: VM Pitfall Tests\n")
;; =============================================================================

;; 1. Shared state across multiple contexts in the same VM
(let* ((run (lambda (expr)
              (let* ((expanded (macroexpand expr))
                     (optimized (optimize expanded))
                     (code (generate-bytecode (compile optimized)))
                     (ctx (vm:init-context code)))
                (vm:vm-run ctx)))))
  (repl:init-globals)
  (display "Testing: shared state across contexts\n")
  (run '(define x 42))
  (let ((res (run 'x)))
    (if (equal? res 42)
        (begin (display "    PASS: shared global x=42\n") (set! *pass-count* (+ *pass-count* 1)))
        (begin (format #t "    FAIL: expected 42, got ~s\n" res) (exit 1))))
  (run '(set! x 100))
  (let ((res (run 'x)))
    (if (equal? res 100)
        (begin (display "    PASS: shared global x updated to 100\n") (set! *pass-count* (+ *pass-count* 1)))
        (begin (format #t "    FAIL: expected 100, got ~s\n" res) (exit 1)))))

(test "closure mutation"
      '(let ((x 10))
         (let ((f (lambda () (set! x (+ x 1)) x)))
           (f)
           (f)
           (f)))
      13)

(test "variable shadowing"
      '(let ((x 10))
         (let ((x 20))
           (+ x x)))
      40)

(test "define vs let shadowing"
      '(begin
         (define x 100)
         (let ((x 10))
           x))
      10)

(test "mutual recursion"
      '(begin
         (define e (lambda (n) (if (= n 0) #t (o (- n 1)))))
         (define o (lambda (n) (if (= n 0) #f (e (- n 1)))))
         (e 10))
      #t)


(test "nested closure capture (label-map bug)"
      '(begin
         (define (for-each func lst)
           (if (not (null? lst))
               (begin (func (car lst))
                      (for-each func (cdr lst)))))
         (define (map proc items)
           (if (null? items)
               '()
               (cons (proc (car items))
                     (map proc (cdr items)))))
         (let* ((all-code '((const r0 3) (ret))) (label-map (make-eq-hashtable)) (final-code '()))
           (for-each
             (lambda (inst)
               (if (not (eq? (car inst) 'label))
                   (let ((resolved
                           (map (lambda (x)
                                  (if (and (symbol? x) (hashtable-contains? label-map x))
                                      (hashtable-ref label-map x)
                                      x))
                                inst)))
                     (set! final-code (cons (list->vector resolved) final-code)))))
             all-code)
           final-code))
      '(#(ret) #(const r0 3)))

;; =============================================================================
(display "\n>>> Section 9: REPL Session Tests\n")
;; =============================================================================

(let ((input-str "(define x 10)\n(+ x 5)\n(define y 20)\n(+ x y)\n")
      (expected-outputs '("15" "30")))
  (let ((output (with-output-to-string
                  (lambda ()
                    (with-input-from-string input-str run-repl)))))
    (let loop ((out expected-outputs))
      (if (null? out)
          (begin (display "  PASS: Multiple Input Session\n") (set! *pass-count* (+ *pass-count* 1)))
          (if (string-scan output (car out))
              (loop (cdr out))
              (begin
                (format #t "  FAIL: Multiple Input Session: Expected output to contain ~s\n" (car out))
                (format #t "  Got:\n~a\n" output)
                (exit 1)))))))

;; =============================================================================

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
