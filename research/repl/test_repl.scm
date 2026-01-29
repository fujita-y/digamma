;;; Test for REPL logic
(load "./research/repl/repl.scm")

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (format #t "Testing: ~a\n" name)
  (let* ((expanded (macroexpand expr))
         (optimized (op:optimize expanded))
         (code (cp:compile optimized))
         (vm (vm:init-vm))
         (ctx (vm:init-context vm code)))
    (repl:init-globals vm)
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
(display "\n>>> Section 1: Basic Integration\n")
;; =============================================================================

(test "simple addition" '(+ 1 2) 3)
(test "let-binding" '(let ((x 10)) (+ x 20)) 30)
(test "define-syntax (syntax-rules)" '(define-syntax foo (syntax-rules () ((foo x) (+ x 1)))) 'defined)
(test "let-syntax (syntax-rules)" '(let-syntax ((foo (syntax-rules () ((foo x) (+ x 1))))) (foo 10)) 11)
(test "if-expression" '(if (= 0 0) 'yes 'no) 'yes)
(test "lambda application" '((lambda (x) (* x x)) 5) 25)

;; =============================================================================
(display "\n>>> Section 2: Macro and Hygiene\n")
;; =============================================================================

(test "my-or macro"
      '(begin
         (define-syntax my-or
           (syntax-rules ()
             ((_ a b)
              (let ((t a))
                (if t t b)))))
         (my-or #t #f))
      #t)

(test "hygienic swap macro"
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

(test "operator shadowing hygiene"
      '(let-syntax ((foo
                    (syntax-rules ()
                      ((_ expr) (+ expr 1)))))
         (let ((+ *))
           (foo 3)))
      4)

;; =============================================================================
(display "\n>>> Section 3: Internal Definitions\n")
;; =============================================================================

(test "lambda internal define" '((lambda (x) (define y 1) (+ x y)) 10) 11)

;; =============================================================================
(display "\n>>> Section 4: Syntax-case Tests\n")
;; =============================================================================

(test "reverse-params (syntax-case)"
      '(begin
         (define-syntax reverse-params
           (lambda (x)
             (syntax-case x ()
               ((_ a b c) (syntax (list c b a))))))
         (reverse-params 1 2 3))
      '(3 2 1))

(test "r6rs-or (syntax-case recursion)"
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

(test "identifier macro (p.car)"
      '(begin
         (define-syntax p.car
           (lambda (x)
             (syntax-case x ()
               [(_ . rest) (syntax ((car p) . rest))]
               [_ (syntax (car p))])))
         (define p (list + 1 2 3))
         (p.car 4 5))
      9)

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
(display "\n>>> Section 7: Quasiquote Tests\n")
;; =============================================================================

(test "quasiquote: atom" '`a 'a)
(test "quasiquote: simple list" '`(1 2 3) '(1 2 3))
(test "quasiquote: simple unquote" '`(a ,2 c) '(a 2 c))
(test "quasiquote: unquote expression" '`(a ,(+ 1 1) c) '(a 2 c))
(test "quasiquote: splicing end" '`(a ,@'(1 2)) '(a 1 2))
(test "quasiquote: multiple splicing" '`(,@'(1 2) ,@'(3 4)) '(1 2 3 4))
(test "quasiquote: R7RS combo" '`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))

;; =============================================================================
(display "\n>>> Section 8: Advanced Hygiene\n")
;; =============================================================================

(test "hygiene: nested (blue/red)"
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

(test "hygiene: local macro capture definition site"
      '(let ((x 1))
         (let-syntax ((get-x (syntax-rules () ((_) x))))
           (let ((x 2))
             (get-x))))
      1)

(test "hygiene: shadowing core lambda with macro"
      '(begin
         (define-syntax my-lambda
           (syntax-rules ()
             ((_ (v) body) (lambda (v) body))))
         (let-syntax ((lambda (syntax-rules () ((_ args body) 'captured))))
           ((my-lambda (y) y) 42)))
      42)

;; =============================================================================
(display "\n>>> Section 9: R6RS Extra Features\n")
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

;; =============================================================================
(display "\n>>> Section 10: R7RS Extra Features\n")
;; =============================================================================

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
(display "\n>>> Section 5: Pitfall Tests\n")
;; =============================================================================

;; 1. Shared state across multiple contexts in the same VM
(let* ((vm (vm:init-vm))
       (_ (repl:init-globals vm))
       (run (lambda (expr)
              (let* ((expanded (macroexpand expr))
                     (optimized (op:optimize expanded))
                     (code (cp:compile optimized))
                     (ctx (vm:init-context vm code)))
                (vm:vm-run ctx)))))
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

;; =============================================================================
(display "\n>>> Section 6: Multiple Input Session\n")
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
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
