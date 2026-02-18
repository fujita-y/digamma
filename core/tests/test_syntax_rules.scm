;; test_syntax_rules.scm
;; Test suite for the R6RS/R7RS macro expansion system.

(load "../core.scm")

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

(define (test-eval name expr expected)
  (let ((result (eval (macroexpand expr) (interaction-environment))))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

(define (test-eval-strip name expr expected)
  (let ((result (eval (macroexpand expr 'strip) (interaction-environment))))
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
;; macroexpand
;; =============================================================================
(display "\n>>> macroexpand\n")

;; Test 1: Simple macro

(test "Register my-or"
      '(define-syntax my-or
         (syntax-rules ()
           ((_ a b)
            (let ((t a))
              (if t t b)))))
      ''defined)

(test "Expand my-or" '(my-or #t #f) '(let ((t #t)) (if t t #f)))

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

(test "Expand (my-and)" '(my-and) '#t)
(test "Expand (my-and 1)" '(my-and 1) '1)
(test "Expand (my-and 1 2 3)" '(my-and 1 2 3) '(if 1 (if 2 3 #f) #f))

;; Test 3: List literal in pattern
(macroexpand '(define-syntax method
                (syntax-rules (=>)
                  ((_ (name args ...) => body)
                   (define (name args ...) body)))))

(test "Expand method with literal =>"
      '(method (add x y) => (+ x y))
      '(define add (lambda (x y) (+ x y))))

;; Test 4: Nested expansion (let uses lambda)
;; Note: our expand function handles let, lambda etc.
(test "Lambda identity" '(lambda (x) x) '(lambda (x) x))

;; =============================================================================
;; hygiene
;; =============================================================================
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
      (display "FAIL: Hygiene test - 'temp' was captured.\n"))
    (begin
      (set! *pass-count* (+ *pass-count* 1))
      (display "PASS: Hygiene test - 'temp' appeared to be renamed (or at least different).\n")))

;; =============================================================================
;; local macros
;; =============================================================================
(display "\n>>> local macros\n")

;; 1. let-syntax binding
(test "let-syntax simple"
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (m 1))
      '(+ 1 1))

;; 2. let-syntax shadowing
(test "let-syntax shadowing"
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (let-syntax ((m (syntax-rules () ((m x) (+ 2 2)))))
           (m 1)))
      '(+ 2 2))

;; 3. letrect-syntax mutual recursion (safe)
(test "letrec-syntax mutual recursion safe"
      '(letrec-syntax ((f (syntax-rules () ((_) (g))))
                       (g (syntax-rules () ((_) 'ok))))
         (f))
      ''ok)

;; letrec-syntax mutual recursion 2
(test "letrec-syntax mutual recursion 2"
      '(letrec-syntax ((a (syntax-rules () ((_) (b))))
                       (b (syntax-rules () ((_) (begin 1 2)))))
         (a))
      '(begin 1 2))

;; let*-syntax scoping
(test "let*-syntax sequential visibility"
      '(let*-syntax ((a (syntax-rules () ((_) (begin 1 2))))
                     (b (syntax-rules () ((_) (a)))))
         (b))
      '(begin 1 2))

;; let*-syntax shadowing
(test "let*-syntax shadowing"
      '(let-syntax ((a (syntax-rules () ((_) 'outer))))
         (let*-syntax ((a (syntax-rules () ((_) 'inner))))
           (a)))
      ''inner)

;; --- Chibi Tests (R5RS 4.3) ---

;; "let-syntax" scoping
(test "Chibi let-syntax scoping"
      '(let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
           (let ((x 'inner))
             (m))))
      '(let ((x 'outer))
         (let ((x 'inner)) x)))

;; "letrec-syntax" R5RS 4.3.1 - my-or
(test "Chibi letrec-syntax my-or"
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
      '(let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
         (let ((temp x))
           (if temp temp
               (let ((temp (let temp)))
                 (if temp temp
                     (let ((temp (if y)))
                       (if temp temp
                           y))))))))

;; --- Gauche Tests ---

;; let-syntax (multi)
(test "Gauche let-syntax multi"
      '(let ((+ *))
         (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x ?x))))
                      (b (syntax-rules () ((_ ?x) (* ?x ?x)))))
           (let ((* -)
                 (+ /))
             (a (b 3)))))
      '(let ((+ *)) (let ((* -) (+ /)) (+ (* 3 3) (* 3 3)))))

;; let-syntax (nest)
(test "Gauche let-syntax nest"
      '(let-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
         (let-syntax ((a (syntax-rules ()
                           ((_ ?x ?y ...) (a ?y ...))
                           ((_) 2))))
           (a 8 9 10)))
      '(+ 9 10))

;; --- R5RS Pitfall Tests ---

;; Pitfall 3.1
(test-eval "Pitfall 3.1: Hygiene with shadowed global operator"
           '(let-syntax ((foo
                          (syntax-rules ()
                            ((_ expr) (+ expr 1)))))
              (let ((+ *))
                (foo 3)))
           4)

;; Pitfall 3.2
(test-eval "Pitfall 3.2: let-syntax inside let with begin and cond"
           '(let-syntax ((foo (syntax-rules ()
                                ((_ var) (define var 1)))))
              (let ((x 2))
                (begin (define foo +))
                (cond (else (foo x)))
                x))
           2)

;; Pitfall 3.3
(test-eval "Pitfall 3.3: Nested let-syntax hygiene"
      '(let ((x 1))
         (let-syntax
             ((foo (syntax-rules ()
                     ((_ y) (let-syntax
                                ((bar (syntax-rules ()
                                        ((_) (let ((x 2)) y)))))
                              (bar))))))
           (foo x)))
           1)

;; Pitfall 3.4
(test-eval "Pitfall 3.4: let-syntax with no clauses"
           '(let-syntax ((x (syntax-rules ()))) 1)
           1)

;; Pitfall 8.1
(test-eval "Pitfall 8.1: named let with name -"
           '(let - ((n (- 1))) n)
           -1)

;; Pitfall 8.3 (R6RS)
(test-eval-strip "Pitfall 8.3: let-syntax and local define"
                 '(let ((x 1))
                    (let-syntax ((foo (syntax-rules () ((_) 2))))
                      (define x (foo))
                      3)
                    x)
                 2)

;; =============================================================================
;; r7rs
;; =============================================================================
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
(test "be-like-begin1" '(sequence1 0 1 2 3) '(begin 0 1 2 3))

;; 2. be-like-begin2 (ellipsis escape)
(macroexpand '(define-syntax be-like-begin2
                (syntax-rules ()
                  ((be-like-begin2 name)
                   (define-syntax name
                     (... (syntax-rules ()
                            ((name expr ...)
                             (begin expr ...)))))))))
(macroexpand '(be-like-begin2 sequence2))
(test "be-like-begin2" '(sequence2 1 2 3 4) '(begin 1 2 3 4))

;; 3. Ellipsis escape via literals
(macroexpand '(define-syntax be-like-begin3
                (syntax-rules ()
                  ((be-like-begin3 name)
                   (define-syntax name
                     (syntax-rules dots ()
                       ((name expr dots)
                        (begin expr dots))))))))
(macroexpand '(be-like-begin3 sequence3))
(test "be-like-begin3" '(sequence3 2 3 4 5) '(begin 2 3 4 5))

;; 4. Ellipsis escape in output
(macroexpand '(define-syntax elli-esc-1
                (syntax-rules ()
                  ((_)
                   '(... ...))
                  ((_ x)
                   '(... (x ...)))
                  ((_ x y)
                   '(... (... x y))))))

(test "elli-esc-1 no args" '(elli-esc-1) ''...)
(test "elli-esc-1 1 arg" '(elli-esc-1 100) ''(100 ...))
(test "elli-esc-1 2 args" '(elli-esc-1 100 200) ''(... 100 200))

;; 5. Underscore
(macroexpand '(define-syntax underscore
                (syntax-rules ()
                  ((foo _) '_))))
(test "underscore match" '(underscore foo) ''_)

;; 6. Jabberwocky (nested definition)
(macroexpand '(define-syntax jabberwocky
                (syntax-rules ()
                  ((_ hatter)
                   (begin
                     (define march-hare 42)
                     (define-syntax hatter
                       (syntax-rules ()
                         ((_) march-hare))))))))

(test "jabberwocky expansion structure"
      '(jabberwocky mad-hatter)
      '(begin (define march-hare 42) 'defined))

;; 7. Literal priority over ellipsis
(macroexpand '(define-syntax elli-lit-1
                (syntax-rules ... (...)
                  ((_ x)
                   '(x ...)))))
(test "Literal ... priority" '(elli-lit-1 100) ''(100 ...))

;; --- Gauche Tests ---

(macroexpand '(define-syntax simple (syntax-rules ()
                                      ((_ "a" ?a) (a ?a))
                                      ((_ "b" ?a) (b ?a))
                                      ((_ #f ?a)  (c ?a))
                                      ((_ #(1 2) ?a) (e ?a))
                                      ((_ ?b ?a)  (f ?a ?b)))))

(test "simple literal match 1" '(simple "a" z) '(a z))
(test "simple literal match 2" '(simple #f z) '(c z))
(test "simple vector match" '(simple #(1 2) z) '(e z))
(test "simple vector non-match" '(simple #(2 1) z) '(f z #(2 1)))


(macroexpand '(define-syntax underbar (syntax-rules ()
                                        [(_) 0]
                                        [(_ _) 1]
                                        [(_ _) 2]
                                        [(_ _ _) 3]
                                        [(_ _ _ _ . _) 'many])))
(test "underbar 0" '(underbar) 0)
(test "underbar 1" '(underbar a) 1)
(test "underbar 3" '(underbar a b c) ''many)
(test "underbar many" '(underbar a b c d) ''many)

(macroexpand '(define-syntax repeat (syntax-rules ()
                                      ((_ 0 (?a ?b) ...)     ((?a ...) (?b ...)))
                                      ((_ 1 (?a ?b) ...)     (?a ... ?b ...))
                                      ((_ 2 (?a ?b) ...)     (?a ... ?b ... ?a ...)))))

(test "repeat 0" '(repeat 0 (a b) (c d) (e f)) '((a c e) (b d f)))
(test "repeat 1" '(repeat 1 (a b) (c d) (e f)) '(a c e b d f))

(macroexpand '(define-syntax repeat2 (syntax-rules () ;r7rs
                                       ((_ 0 (?a ?b ... ?c))    (?a (?b ...) ?c))
                                       ((_ 1 (?a ?b ... ?c ?d)) (?a (?b ...) ?c ?d))
                                       ((_ 2 (?a ?b ... . ?c))  (?a (?b ...) ?c))
                                       ((_ ?x ?y) 'ho))))

(test "repeat2 0" '(repeat2 0 (a b c d e f g)) '(a (b c d e f) g))
(test "repeat2 0 empty middle" '(repeat2 0 (a b)) '(a () b))
(test "repeat2 0 fail" '(repeat2 0 (a)) ''ho)

(macroexpand '(define-syntax nest1 (syntax-rules ()
                                     ((_ (?a ...) ...)        ((?a ... z) ...)))))

(test "nest1"
      '(nest1 (a) (b c d) (e f g h i) () (j))
      '((a z) (b c d z) (e f g h i z) (z) (j z)))

(macroexpand '(define-syntax nest4 (syntax-rules () ; r7rs parameter list
                                    ((_ ((?a ?b ... ?c) ... ?d))
                                     ((?a ...) ((?b ...) ...) (?c ...) ?d)))))

(test "nest4"
      '(nest4 ((a b c) (d e) (f g h i j) (k l m)))
      '((a d f) ((b) () (g h i)) (c e j) (k l m)))

;; mixlevel
(macroexpand '(define-syntax mixlevel1 (syntax-rules ()
                                         ((_ (?a ?b ...)) ((?a ?b) ...)))))
(test "mixlevel1" '(mixlevel1 (1 2 3 4 5 6)) '((1 2) (1 3) (1 4) (1 5) (1 6)))

(macroexpand '(define-syntax hygiene (syntax-rules ()
                                       ((_ ?a) (+ ?a 1)))))
;; We can't evaluate, but we test structure.
(test "hygiene - + should be renamed" '(let ((+ *)) (hygiene 2)) '(let ((+ *)) (+ 2 1)))

(macroexpand '(define-syntax vect1 (syntax-rules ()
                                     ((_ #(?a ...)) (?a ...))
                                     ((_ (?a ...))  #(?a ...)))))
(test "vect1 decode" '(vect1 #(1 2 3 4 5)) '(1 2 3 4 5))
(test "vect1 encode" '(vect1 (1 2 3 4 5)) '#(1 2 3 4 5))

(macroexpand '(define-syntax dot1 (syntax-rules ()
                                   ((_ (?a . ?b)) (?a ?b))
                                   ((_ ?loser) #f))))
(test "dot1 pair" '(dot1 (1 . 2)) '(1 2))
(test "dot1 list" '(dot1 (1 2)) '(1 (2)))

(macroexpand '(define-syntax dot3 (syntax-rules ()
                                   ((_ (?a ...) ?b) (?a ... . ?b)))))
(test "dot3" '(dot3 (1 2) 3) '(1 2 . 3))

(macroexpand '(define-syntax tailmatch-rules1
                (syntax-rules ()
                  ((_ first ... last . rest)
                   rest))))
(test "tailmatch-rules-test1" '(tailmatch-rules1 1 2 3 . 4) 4)

(macroexpand '(define-syntax tailmatch-rules2
                (syntax-rules ()
                  ((_ first ... . rest)
                   rest))))
(test "tailmatch-rules-test2" '(tailmatch-rules2 1 2 3 . 4) 4)

;; =============================================================================
;; chez scheme
;; =============================================================================
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

(test "Chez Scheme hygiene test: or, if, t shadowing"
      '(let ((if #f))
         (let ((t 'okay))
           (chez-or if t)))
      '(let ((if #f))
         (let ((t 'okay))
           (let ((t if))
             (if t t t)))))

;; =============================================================================
;; corner cases
;; =============================================================================
(display "\n>>> corner cases\n")

;; Corner Case 1: Dotted List Matching
;; Matches (1 . 2) and (1 2)
(macroexpand '(define-syntax dotted
                (syntax-rules ()
                  ((_ (a . b)) (cons a b)))))

(test "dotted pair match" '(dotted (1 . 2)) '(cons 1 2))
(test "dotted list match" '(dotted (1 2)) '(cons 1 (2)))

;; Corner Case 2: Vector Patterns w/ Ellipsis
;; Verify #(a ...) bindings and template expansion
(macroexpand '(define-syntax vector-ellipses
                (syntax-rules ()
                  ((_ #(a ...))
                   (list a ...)))))

(test "vector ellipsis match" '(vector-ellipses #(1 2 3)) '(list 1 2 3))

;; Corner Case 3: Empty Vector
;; Verify #() literal matching
(macroexpand '(define-syntax empty-vec
                (syntax-rules ()
                  ((_ #()) 'empty)
                  ((_ x) 'not-empty))))

(test "empty vector match" '(empty-vec #()) ''empty)
(test "non-empty vector fallback" '(empty-vec #(1)) ''not-empty)

;; Corner Case 4: Ellipsis as Literal
;; Verify ... can be used as a literal to match identifier ...
(macroexpand '(define-syntax ellipsis-literal
                (syntax-rules (...)
                  ((_ ...) 'ellipsis-found)
                  ((_ x) 'not-ellipsis))))

(test "ellipsis literal match" '(ellipsis-literal ...) ''ellipsis-found)
(test "ellipsis literal non-match" '(ellipsis-literal other) ''not-ellipsis)

;; Corner Case 5: Symbolic Literals
;; Verify identifier matching works for symbols like =>
(macroexpand '(define-syntax symbol-literal
                (syntax-rules (=>)
                  ((_ =>) 'arrow)
                  ((_ x) 'not-arrow))))

(test "symbol literal match" '(symbol-literal =>) ''arrow)
(test "symbol literal non-match" '(symbol-literal other) ''not-arrow)

;; =============================================================================
;; local syntax scoping
;; =============================================================================
(display "\n>>> local syntax scoping\n")

;; User Case 1: letrec-syntax scoping
(test "User requested: letrec-syntax scoping case"
      '(let ((f (lambda (x) (+ x 1)))) ; an outer variable binding for 'f'
         (letrec-syntax ((f (syntax-rules () ((_ x) x))) ; local macro 'f'
                         (g (syntax-rules () ((_ x) (f x))))) ; local macro 'g' calls 'f'
           (list (f 1) (g 1))))
      '(let ((f (lambda (x) (+ x 1))))
         (list 1 1)))

;; User Case 2: let-syntax scoping
(test "User requested: let-syntax scoping case"
      '(let ((f (lambda (x) (+ x 1)))) ; an outer variable binding for 'f'
         (let-syntax ((f (syntax-rules () ((_ x) x))) ; local macro 'f'
                      (g (syntax-rules () ((_ x) (f x))))) ; local macro 'g' calls 'f'
           (list (f 1) (g 1))))
      '(let ((f (lambda (x) (+ x 1))))
         (list 1 (f 1))))

;; =============================================================================
;; let* and letrec*
;; =============================================================================
(display "\n>>> let* and letrec*\n")

(test "let* sequential binding"
      '(let* ((x 1) (y (+ x 1))) (+ x y))
      '(let ((x 1)) (let ((y (+ x 1))) (+ x y))))

(test "letrec* sequential recursive binding"
      '(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))
      '(let ((f #f))
         (set! f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))
         (f 5)))

;; =============================================================================
;; internal define (R6RS)
;; =============================================================================
(display "\n>>> internal define (R6RS)\n")

;; Internal define in lambda
(test "lambda internal define simple"
      '(lambda (x) (define y 1) (+ x y))
      '(lambda (x) (let ((y #f)) (set! y 1) (+ x y))))

;; Internal define with function syntax in lambda
(test "lambda internal define function syntax"
      '(lambda (x) (define (f n) (* n 2)) (f x))
      '(lambda (x) (let ((f #f)) (set! f (lambda (n) (* n 2))) (f x))))

;; Multiple internal defines in lambda
(test "lambda multiple internal defines"
      '(lambda (x) (define a 1) (define b 2) (+ x a b))
      '(lambda (x) (let ((a #f) (b #f)) (set! a 1) (set! b 2) (+ x a b))))

;; Internal define in let
(test "let internal define"
      '(let ((x 1)) (define y 2) (+ x y))
      '(let ((x 1)) (let ((y #f)) (set! y 2) (+ x y))))

;; Internal define in let*
(test "let* internal define"
      '(let* ((a 1) (b 2)) (define c 3) (+ a b c))
      '(let ((a 1)) (let ((b 2)) (let ((c #f)) (set! c 3) (+ a b c)))))

;; Internal define in letrec
(test "letrec internal define"
      '(letrec ((f (lambda (x) x))) (define (g y) y) (f (g 1)))
      '(let ((f #f) (g #f)) (set! f (lambda (x) x)) (set! g (lambda (y) y)) (f (g 1))))

;; Internal define in letrec*
(test "letrec* internal define merged"
      '(letrec* ((a 1) (b 2)) (define c 3) (+ a b c))
      '(let ((a #f) (b #f) (c #f)) (set! a 1) (set! b 2) (set! c 3) (+ a b c)))

;; Internal defines in nested begin
(test "let internal define 2"
      '(let ((x 1)) (define y 2) (define z 3) (+ x y z))
      '(let ((x 1)) (let ((y #f) (z #f)) (set! y 2) (set! z 3) (+ x y z))))

;; No internal defines should not add letrec*
(test "lambda without internal define unchanged"
      '(lambda (x) (+ x 1))
      '(lambda (x) (+ x 1)))


;; =============================================================================
;; lambda*
;; =============================================================================
(display "\n>>> lambda*\n")

;; Helper to decompose dotted/improper lists into a flat list of identifiers
;; and then call the original lambda* logic.
(macroexpand '(define-syntax lambda*-helper
                (syntax-rules ()
                  ;; Case 1: Reached the end of a dotted pair (the 'rest' variable)
                  [(_ (h . t) (id ...) body)
                   (lambda*-helper t (id ... h) body)]
                  ;; Case 2: The tail is an identifier (improper list end) or empty
                  [(_ rest (id ...) (b0 b1 ...))
                   (lambda args
                     (let-ids args (id ... rest) b0 b1 ...))])))

;; Sequential binding helper with O(N) traversal optimization
(macroexpand '(define-syntax let-ids
                (syntax-rules ()
                  ;; Base case: 1 identifier (the 'rest' or last one)
                  ;; We bind the last identifier to the remaining list.
                  [(_ ls (last) b0 b1 ...)
                   (let ([last ls]) b0 b1 ...)]
                  ;; Recursive step
                  ;; We bind the current head to id0, and effectively peel off the cdr to a temp variable 'next'.
                  ;; 'next' is then passed to the recursive call, avoiding nested (cdr (cdr ...)) chains.
                  [(_ ls (id0 id1 ...) b0 b1 ...)
                   (let ([id0 (car ls)]
                         [next (cdr ls)])
                     (let-ids next (id1 ...) b0 b1 ...))])))

;; Main macro
(macroexpand '(define-syntax lambda*
                (syntax-rules ()
                  ;; Handle standard proper lists: (lambda* (x y) ...)
                  [(_ (id ...) b0 b1 ...)
                   (lambda (id ...) b0 b1 ...)]
                  ;; Handle improper/dotted lists: (lambda* (x . y) ...)
                  [(_ (h . t) b0 b1 ...)
                   (lambda*-helper (h . t) () (b0 b1 ...))]
                  ;; Handle single rest argument: (lambda* args ...)
                  [(_ rest b0 b1 ...)
                   (lambda rest b0 b1 ...)])))

;; Test 1: Proper list arguments
(test-eval "lambda* proper list (2 args)"
           '((lambda* (x y) (list x y)) 1 2)
           '(1 2))

;; Test 2: Standard rest argument
(test-eval "lambda* standard rest arg"
           '((lambda* args args) 1 2 3)
           '(1 2 3))

;; Test 3: Dotted list (1 fixed, rest)
(test-eval "lambda* dotted list (1 fixed)"
           '((lambda* (x . y) (list x y)) 1 2 3)
           '(1 (2 3)))

;; Test 4: Dotted list (2 fixed, rest)
(test-eval "lambda* dotted list (2 fixed)"
           '((lambda* (x y . z) (list x y z)) 1 2 3 4)
           '(1 2 (3 4)))

;; Test 5: Dotted list exact match (rest empty)
(test-eval "lambda* dotted list (rest empty)"
           '((lambda* (x . y) (list x y)) 1)
           '(1 ()))

(newline)
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
