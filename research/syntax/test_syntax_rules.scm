;; test_syntax_rules.scm
;; Test suite for the R6RS/R7RS macro expansion system.
;;
;; Test categories:
;;   - Macro expansion: define-syntax, syntax-rules, recursive macros
;;   - Hygiene: variable capture prevention, alpha-renaming
;;   - Local macros: let-syntax, letrec-syntax, let*-syntax scoping
;;   - R7RS features: ellipsis escape, literals, vectors
;;   - Binding forms: let, let*, letrec, letrec*, named let
;;   - Internal defines: R6RS-style conversion to letrec*

(load "macroexpand.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (assert-equal expected actual msg)
  (if (equal? expected actual)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display msg) (newline))
      (begin 
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display msg) (newline)
        (display "  Expected: ") (display expected) (newline)
        (display "  Actual:   ") (display actual) (newline))))

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
          (display expected)
          (newline)
          (display "  Actual:   ")
          (display expanded)
          (newline)))))

(define (test-eval expected expr msg)
  (let ((expanded (eval (macroexpand expr 'strip) (interaction-environment))))
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
          (display expected)
          (newline)
          (display "  Actual:   ")
          (display expanded)
          (newline)))))


(display "\n>>> macroexpand\n")

;; Test 1: Simple macro
(define-syntax my-or
  (syntax-rules ()
    ((_ a b)
     (let ((t a))
       (if t t b)))))

(assert-equal ''defined
              (macroexpand '(define-syntax my-or
                              (syntax-rules ()
                                ((_ a b)
                                 (let ((t a))
                                   (if t t b))))))
              "Register my-or")

(test '(let ((t #t)) (if t t #f))
      '(my-or #t #f)
      "Expand my-or")

;; Test 2: Recursive macro (my-and)
;; (and) -> #t
;; (and x) -> x
;; (and x y ...) -> (if x (and y ...) #f)

(macroexpand '(define-syntax my-and
                (syntax-rules ()
                  ((_) #t)
                  ((_ x) x)
                  ((_ x y ...)
                   (if x (my-and y ...) #f)))))

(assert-equal '#t
              (macroexpand '(my-and))
              "Expand (my-and)")

(assert-equal '1
              (macroexpand '(my-and 1))
              "Expand (my-and 1)")

(assert-equal '(if 1 (if 2 3 #f) #f)
              (macroexpand '(my-and 1 2 3))
              "Expand (my-and 1 2 3)")

;; Test 3: List literal in pattern
(macroexpand '(define-syntax method
                (syntax-rules (=>)
                  ((_ (name args ...) => body)
                   (define (name args ...) body)))))

(assert-equal '(define (add x y) (+ x y))
              (macroexpand '(method (add x y) => (+ x y)))
              "Expand method with literal =>")

;; Test 4: Nested expansion (let uses lambda)
;; Note: our expand function handles let, lambda etc.
(test '(lambda (x) x)
      '(lambda (x) x)
      "Lambda identity")

(display "\n>>> hygiene\n")

;; Hygiene Test 1: Variable Capture
;; The macro introduces 'temp'. We use 'temp' as an argument.
;; If unhygienic, the macro's 'temp' will shadow the user's 'temp'.

(macroexpand '(define-syntax swap
                (syntax-rules ()
                  ((_ a b)
                   (let ((temp a))
                     (set! a b)
                     (set! b temp))))))

(define result (macroexpand '(let ((temp 1) (other 2))
                               (swap temp other))))

(display "Expansion result: ")
(display result)
(newline)

;; In a hygienic expansion, the inner 'temp' should be renamed, e.g., 'temp.1'.
;; So we check that the let-bound variable is NOT 'temp'.

(define inner-let (caddr result)) ;; (let (...) (swap ...)) -> (let (...) (let ...))
(define inner-binding (car (cadr inner-let))) ;; (temp temp)
(define inner-var (car inner-binding))

(if (eq? inner-var 'temp)
    (begin
      (set! *fail-count* (+ *fail-count* 1))
      (display "FAIL: Hygiene test - 'temp' was captured.\n")
      (assert-equal "not temp" inner-var "Inner variable should be renamed"))
    (begin
      (set! *pass-count* (+ *pass-count* 1))
      (display "PASS: Hygiene test - 'temp' appeared to be renamed (or at least different).\n")))

(display "\n>>> local macros\n")

;; 1. let-syntax binding
(test '(+ 1 1)
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (m 1))
      "let-syntax simple")

;; 2. let-syntax shadowing
(test '(+ 2 2)
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (let-syntax ((m (syntax-rules () ((m x) (+ 2 2)))))
           (m 1)))
      "let-syntax shadowing")

;; 3. letrect-syntax mutual recursion (safe)
(test ''ok
      '(letrec-syntax ((f (syntax-rules () ((_) (g))))
                       (g (syntax-rules () ((_) 'ok))))
         (f))
      "letrec-syntax mutual recursion safe")

;; letrec-syntax mutual recursion 2
(test '(begin 1 2)
      '(letrec-syntax ((a (syntax-rules () ((_) (b))))
                       (b (syntax-rules () ((_) (begin 1 2)))))
         (a))
      "letrec-syntax mutual recursion 2")

;; let*-syntax scoping
(test '(begin 1 2)
      '(let*-syntax ((a (syntax-rules () ((_) (begin 1 2))))
                     (b (syntax-rules () ((_) (a)))))
         (b))
      "let*-syntax sequential visibility")

;; let*-syntax shadowing
(test ''inner
      '(let-syntax ((a (syntax-rules () ((_) 'outer))))
         (let*-syntax ((a (syntax-rules () ((_) 'inner))))
           (a)))
      "let*-syntax shadowing")

;; --- Chibi Tests (R5RS 4.3) ---

;; "let-syntax" scoping
(test '(let ((x 'outer))
         (let ((x 'inner)) x))
      '(let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
           (let ((x 'inner))
             (m))))
      "Chibi let-syntax scoping")

;; "letrec-syntax" R5RS 4.3.1 - my-or
(test '(let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
         (let ((temp x))
           (if temp temp
               (let ((temp (let temp)))
                 (if temp temp
                     (let ((temp (if y)))
                       (if temp temp
                           y)))))))
      '(letrec-syntax
           ((my-or (syntax-rules ()
                     ((my-or) #f)
                     ((my-or e) e)
                     ((my-or e1 e2 ...)
                      (let ((temp e1))
                        (if temp
                            temp
                            (my-or e2 ...)))))))
         (let ((x #f)
               (y 7)
               (temp 8)
               (let odd?)
               (if even?))
           (my-or x
                  (let temp)
                  (if y)
                  y)))
      "Chibi letrec-syntax my-or")

;; --- Gauche Tests ---

;; let-syntax (multi)
(test '(let ((+ *)) (let ((* -) (+ /)) (+ (* 3 3) (* 3 3))))
      '(let ((+ *))
         (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x ?x))))
                      (b (syntax-rules () ((_ ?x) (* ?x ?x)))))
           (let ((* -)
                 (+ /))
             (a (b 3)))))
      "Gauche let-syntax multi")

;; let-syntax (nest)
(test '(+ 9 10)
      '(let-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
         (let-syntax ((a (syntax-rules ()
                           ((_ ?x ?y ...) (a ?y ...))
                           ((_) 2))))
           (a 8 9 10)))
      "Gauche let-syntax nest")

;; --- R5RS Pitfall Tests ---

;; Pitfall 3.1
(test-eval 4
      '(let-syntax ((foo
                     (syntax-rules ()
                       ((_ expr) (+ expr 1)))))
         (let ((+ *))
           (foo 3)))
      "Pitfall 3.1: Hygiene with shadowed global operator")

;; Pitfall 3.2
(test-eval 2
      '(let-syntax ((foo (syntax-rules ()
                           ((_ var) (define var 1)))))
         (let ((x 2))
           (begin (define foo +))
           (cond (else (foo x)))
           x))
      "Pitfall 3.2: let-syntax inside let with begin and cond")

;; Pitfall 3.3
(test-eval 1
      '(let ((x 1))
         (let-syntax
             ((foo (syntax-rules ()
                     ((_ y) (let-syntax
                                ((bar (syntax-rules ()
                                        ((_) (let ((x 2)) y)))))
                              (bar))))))
           (foo x)))
      "Pitfall 3.3: Nested let-syntax hygiene")

;; Pitfall 3.4
(test-eval 1
      '(let-syntax ((x (syntax-rules ()))) 1)
      "Pitfall 3.4: let-syntax with no clauses")

;; Pitfall 8.1
(test-eval -1
      '(let - ((n (- 1))) n)
      "Pitfall 8.1: named let with name -")

;; Pitfall 8.3 (R6RS)
(test-eval 2
      '(let ((x 1))
         (let-syntax ((foo (syntax-rules () ((_) 2))))
           (define x (foo))
           3)
         x)
      "Pitfall 8.3: let-syntax and local define")

(display "\n>>> r7rs\n")

;; 1. be-like-begin1
(macroexpand '(define-syntax be-like-begin1
                (syntax-rules ()
                  ((be-like-begin1 name)
                   (define-syntax name
                     (syntax-rules ()
                       ((name expr (... ...))
                        (begin expr (... ...)))))))))
;; Register sequence1
(macroexpand '(be-like-begin1 sequence1))
(test '(begin 0 1 2 3)
      '(sequence1 0 1 2 3)
      "be-like-begin1")

;; 2. be-like-begin2 (ellipsis escape)
(macroexpand '(define-syntax be-like-begin2
                (syntax-rules ()
                  ((be-like-begin2 name)
                   (define-syntax name
                     (... (syntax-rules ()
                            ((name expr ...)
                             (begin expr ...)))))))))
(macroexpand '(be-like-begin2 sequence2))
(test '(begin 1 2 3 4)
      '(sequence2 1 2 3 4)
      "be-like-begin2")

;; 3. Ellipsis escape via literals
(macroexpand '(define-syntax be-like-begin3
                (syntax-rules ()
                  ((be-like-begin3 name)
                   (define-syntax name
                     (syntax-rules dots ()
                       ((name expr dots)
                        (begin expr dots))))))))
(macroexpand '(be-like-begin3 sequence3))
(test '(begin 2 3 4 5)
      '(sequence3 2 3 4 5)
      "be-like-begin3")

;; 4. Ellipsis escape in output
(macroexpand '(define-syntax elli-esc-1
                (syntax-rules ()
                  ((_)
                   '(... ...))
                  ((_ x)
                   '(... (x ...)))
                  ((_ x y)
                   '(... (... x y))))))

(test ''...
      '(elli-esc-1)
      "elli-esc-1 no args")
(test ''(100 ...)
      '(elli-esc-1 100)
      "elli-esc-1 1 arg")
(test ''(... 100 200)
      '(elli-esc-1 100 200)
      "elli-esc-1 2 args")

;; 5. Underscore
(macroexpand '(define-syntax underscore
                (syntax-rules ()
                  ((foo _) '_))))
(test ''_
      '(underscore foo)
      "underscore match")

;; 6. Jabberwocky (nested definition)
(macroexpand '(define-syntax jabberwocky
                (syntax-rules ()
                  ((_ hatter)
                   (begin
                     (define march-hare 42)
                     (define-syntax hatter
                       (syntax-rules ()
                         ((_) march-hare))))))))

(test '(begin (define march-hare 42) 'defined)
      '(jabberwocky mad-hatter)
      "jabberwocky expansion structure")

;; 7. Literal priority over ellipsis
(macroexpand '(define-syntax elli-lit-1
                (syntax-rules ... (...)
                  ((_ x)
                   '(x ...)))))
(test ''(100 ...)
      '(elli-lit-1 100)
      "Literal ... priority")

;; --- Gauche Tests ---

(macroexpand '(define-syntax simple (syntax-rules ()
                                      ((_ "a" ?a) (a ?a))
                                      ((_ "b" ?a) (b ?a))
                                      ((_ #f ?a)  (c ?a))
                                      ((_ #(1 2) ?a) (e ?a))
                                      ((_ ?b ?a)  (f ?a ?b)))))

(test '(a z) '(simple "a" z) "simple literal match 1")
(test '(c z) '(simple #f z) "simple literal match 2")
(test '(e z) '(simple #(1 2) z) "simple vector match")
(test '(f z #(2 1)) '(simple #(2 1) z) "simple vector non-match")


(macroexpand '(define-syntax underbar (syntax-rules ()
                                        [(_) 0]
                                        [(_ _) 1]
                                        [(_ _) 2]
                                        [(_ _ _) 3]
                                        [(_ _ _ _ . _) 'many])))
(test 0 '(underbar) "underbar 0")
(test 1 '(underbar a) "underbar 1")
(test ''many '(underbar a b c) "underbar 3")
(test ''many '(underbar a b c d) "underbar many")

(macroexpand '(define-syntax repeat (syntax-rules ()
                                      ((_ 0 (?a ?b) ...)     ((?a ...) (?b ...)))
                                      ((_ 1 (?a ?b) ...)     (?a ... ?b ...))
                                      ((_ 2 (?a ?b) ...)     (?a ... ?b ... ?a ...)))))

(test '((a c e) (b d f))
      '(repeat 0 (a b) (c d) (e f))
      "repeat 0")
(test '(a c e b d f)
      '(repeat 1 (a b) (c d) (e f))
      "repeat 1")

(macroexpand '(define-syntax repeat2 (syntax-rules () ;r7rs
                                       ((_ 0 (?a ?b ... ?c))    (?a (?b ...) ?c))
                                       ((_ 1 (?a ?b ... ?c ?d)) (?a (?b ...) ?c ?d))
                                       ((_ 2 (?a ?b ... . ?c))  (?a (?b ...) ?c))
                                       ((_ ?x ?y) 'ho))))

(test '(a (b c d e f) g)
      '(repeat2 0 (a b c d e f g))
      "repeat2 0")
(test '(a () b)
      '(repeat2 0 (a b))
      "repeat2 0 empty middle")
(test ''ho '(repeat2 0 (a)) "repeat2 0 fail")

(macroexpand '(define-syntax nest1 (syntax-rules ()
                                     ((_ (?a ...) ...)        ((?a ... z) ...)))))

(test '((a z) (b c d z) (e f g h i z) (z) (j z))
      '(nest1 (a) (b c d) (e f g h i) () (j))
      "nest1")

(macroexpand '(define-syntax nest4 (syntax-rules () ; r7rs parameter list
                                    ((_ ((?a ?b ... ?c) ... ?d))
                                     ((?a ...) ((?b ...) ...) (?c ...) ?d)))))

(test '((a d f) ((b) () (g h i)) (c e j) (k l m))
      '(nest4 ((a b c) (d e) (f g h i j) (k l m)))
      "nest4")

;; mixlevel
(macroexpand '(define-syntax mixlevel1 (syntax-rules ()
                                         ((_ (?a ?b ...)) ((?a ?b) ...)))))
(test '((1 2) (1 3) (1 4) (1 5) (1 6))
      '(mixlevel1 (1 2 3 4 5 6))
      "mixlevel1")

(macroexpand '(define-syntax hygiene (syntax-rules ()
                                       ((_ ?a) (+ ?a 1)))))
;; We can't evaluate, but we test structure.
(test '(let ((+ *)) (+ 2 1)) '(let ((+ *)) (hygiene 2)) "hygiene - + should be renamed")

(macroexpand '(define-syntax vect1 (syntax-rules ()
                                     ((_ #(?a ...)) (?a ...))
                                     ((_ (?a ...))  #(?a ...)))))
(test '(1 2 3 4 5) '(vect1 #(1 2 3 4 5)) "vect1 decode")
(test '#(1 2 3 4 5) '(vect1 (1 2 3 4 5)) "vect1 encode")

(macroexpand '(define-syntax dot1 (syntax-rules ()
                                   ((_ (?a . ?b)) (?a ?b))
                                   ((_ ?loser) #f))))
(test '(1 2) '(dot1 (1 . 2)) "dot1 pair")
(test '(1 (2)) '(dot1 (1 2)) "dot1 list")

(macroexpand '(define-syntax dot3 (syntax-rules ()
                                   ((_ (?a ...) ?b) (?a ... . ?b)))))
(test '(1 2 . 3) '(dot3 (1 2) 3) "dot3")

(macroexpand '(define-syntax tailmatch-rules1
                (syntax-rules ()
                  ((_ first ... last . rest)
                   rest))))
(test 4 '(tailmatch-rules1 1 2 3 . 4) "tailmatch-rules-test1")

(macroexpand '(define-syntax tailmatch-rules2
                (syntax-rules ()
                  ((_ first ... . rest)
                   rest))))
(test 4 '(tailmatch-rules2 1 2 3 . 4) "tailmatch-rules-test2")

(display "\n>>> chez scheme\n")

;; From https://www.scheme.com/csug7/syntax.html Section 10.3

(macroexpand '(define-syntax chez-or
                (syntax-rules ()
                  ((_) #f)
                  ((_ e) e)
                  ((_ e1 e2 e3 ...)
                   (let ((t e1))
                     (if t t (chez-or e2 e3 ...)))))))

;; The expansion algorithm maintains lexical scoping automatically...
;; (let ([if #f]) (let ([t 'okay]) (or if t))) => okay

(test '(let ((if #f))
         (let ((t 'okay))
           (let ((t if))
             (if t t t))))
      '(let ((if #f))
         (let ((t 'okay))
           (chez-or if t)))
      "Chez Scheme hygiene test: or, if, t shadowing")

(display "\n>>> corner cases\n")

;; Corner Case 1: Dotted List Matching
;; Matches (1 . 2) and (1 2)
(macroexpand '(define-syntax dotted
                (syntax-rules ()
                  ((_ (a . b)) (cons a b)))))

(test '(cons 1 2) '(dotted (1 . 2)) "dotted pair match")
(test '(cons 1 (2)) '(dotted (1 2)) "dotted list match")

;; Corner Case 2: Vector Patterns w/ Ellipsis
;; Verify #(a ...) bindings and template expansion
(macroexpand '(define-syntax vector-ellipses
                (syntax-rules ()
                  ((_ #(a ...))
                   (list a ...)))))

(test '(list 1 2 3) '(vector-ellipses #(1 2 3)) "vector ellipsis match")

;; Corner Case 3: Empty Vector
;; Verify #() literal matching
(macroexpand '(define-syntax empty-vec
                (syntax-rules ()
                  ((_ #()) 'empty)
                  ((_ x) 'not-empty))))

(test ''empty '(empty-vec #()) "empty vector match")
(test ''not-empty '(empty-vec #(1)) "non-empty vector fallback")

;; Corner Case 4: Ellipsis as Literal
;; Verify ... can be used as a literal to match identifier ...
(macroexpand '(define-syntax ellipsis-literal
                (syntax-rules (...)
                  ((_ ...) 'ellipsis-found)
                  ((_ x) 'not-ellipsis))))

(test ''ellipsis-found '(ellipsis-literal ...) "ellipsis literal match")
(test ''not-ellipsis '(ellipsis-literal other) "ellipsis literal non-match")

;; Corner Case 5: Symbolic Literals
;; Verify identifier matching works for symbols like =>
(macroexpand '(define-syntax symbol-literal
                (syntax-rules (=>)
                  ((_ =>) 'arrow)
                  ((_ x) 'not-arrow))))

(test ''arrow '(symbol-literal =>) "symbol literal match")
(test ''not-arrow '(symbol-literal other) "symbol literal non-match")

(display "\n>>> local syntax scoping\n")

;; User Case 1: letrec-syntax scoping
(test '(let ((f (lambda (x) (+ x 1))))
         (list 1 1))
      '(let ((f (lambda (x) (+ x 1)))) ; an outer variable binding for 'f'
         (letrec-syntax ((f (syntax-rules () ((_ x) x))) ; local macro 'f'
                         (g (syntax-rules () ((_ x) (f x))))) ; local macro 'g' calls 'f'
           (list (f 1) (g 1))))
      "User requested: letrec-syntax scoping case")

;; User Case 2: let-syntax scoping
(test '(let ((f (lambda (x) (+ x 1))))
         (list 1 (f 1)))
      '(let ((f (lambda (x) (+ x 1)))) ; an outer variable binding for 'f'
         (let-syntax ((f (syntax-rules () ((_ x) x))) ; local macro 'f'
                      (g (syntax-rules () ((_ x) (f x))))) ; local macro 'g' calls 'f'
           (list (f 1) (g 1))))
      "User requested: let-syntax scoping case")

(display "\n>>> let* and letrec*\n")

(test '(let ((x 1)) (let ((y (+ x 1))) (+ x y)))
      '(let* ((x 1) (y (+ x 1))) (+ x y))
      "let* sequential binding")

(test '(letrec* ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))
      '(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))
      "letrec* sequential recursive binding")

(display "\n>>> internal define (R6RS)\n")

;; Internal define in lambda
(test '(lambda (x) (letrec* ((y 1)) (+ x y)))
      '(lambda (x) (define y 1) (+ x y))
      "lambda internal define simple")

;; Internal define with function syntax in lambda
(test '(lambda (x) (letrec* ((f (lambda (n) (* n 2)))) (f x)))
      '(lambda (x) (define (f n) (* n 2)) (f x))
      "lambda internal define function syntax")

;; Multiple internal defines in lambda
(test '(lambda (x) (letrec* ((a 1) (b 2)) (+ x a b)))
      '(lambda (x) (define a 1) (define b 2) (+ x a b))
      "lambda multiple internal defines")

;; Internal define in let
(test '(let ((x 1)) (letrec* ((y 2)) (+ x y)))
      '(let ((x 1)) (define y 2) (+ x y))
      "let internal define")

;; Internal define in let*
(test '(let ((a 1)) (let ((b 2)) (letrec* ((c 3)) (+ a b c))))
      '(let* ((a 1) (b 2)) (define c 3) (+ a b c))
      "let* internal define")

;; Internal define in letrec
(test '(letrec* ((f (lambda (x) x)) (g (lambda (y) y))) (f (g 1)))
      '(letrec ((f (lambda (x) x))) (define (g y) y) (f (g 1)))
      "letrec internal define")

;; Internal define in letrec*
(test '(letrec* ((a 1) (b 2) (c 3)) (+ a b c))
      '(letrec* ((a 1) (b 2)) (define c 3) (+ a b c))
      "letrec* internal define merged")

;; Internal defines in nested begin
(test '(let ((x 1)) (letrec* ((y 2) (z 3)) (+ x y z)))
      '(let ((x 1)) (define y 2) (define z 3) (+ x y z))
      "let internal define")

;; No internal defines should not add letrec*
(test '(lambda (x) (+ x 1))
      '(lambda (x) (+ x 1))
      "lambda without internal define unchanged")

(newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
