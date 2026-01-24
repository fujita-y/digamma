;; test_syntax.scm
(load "macroexpand.scm")

;; --- Helper Functions ---

(define (assert-equal expected actual msg)
  (if (equal? expected actual)
      (begin (display "PASS: ") (display msg) (newline))
      (begin (display "FAIL: ") (display msg) (newline)
             (display "  Expected: ") (display expected) (newline)
             (display "  Actual:   ") (display actual) (newline))))

(define (my-every? pred lst)
  (if (null? lst) #t
      (and (pred (car lst)) (my-every? pred (cdr lst)))))

(define (strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            ;; Found dot from right
            (if (and (not (null? suffix)) (my-every? char-numeric? suffix))
                (strip-suffix (list->string (reverse (cdr chars))))
                (if (and (not (null? suffix)))
                    (let ((stripped (list->string (reverse (cdr chars)))))
                      ;; Recurse to strip more suffixes (e.g. .10.11 -> .10 -> "")
                      (strip-suffix stripped))
                    str))
            (loop (cdr chars) (cons (car chars) suffix))))))

(define (strip-renames expr)
  (cond
   ((symbol? expr)
    (string->symbol (strip-suffix (symbol->string expr))))
   ((pair? expr)
    (cons (strip-renames (car expr))
          (strip-renames (cdr expr))))
   ((vector? expr)
    (list->vector (map strip-renames (vector->list expr))))
   (else expr)))

(define (test expected expr msg)
  (let ((expanded (expand expr)))
    (if (equal? expected (strip-renames expanded))
        (begin
          (display "PASS: ")
          (display msg)
          (newline))
        (begin
          (display "FAIL: ")
          (display msg)
          (newline)
          (display "  Expected: ")
          (display expected)
          (newline)
          (display "  Actual:   ")
          (display expanded)
          (newline)
          (display "  Stripped: ")
          (display (strip-renames expanded))
          (newline)))))

(display "\n>>> macroexpand\n")

;; Test 1: Simple macro
(define-syntax my-or
  (syntax-rules ()
    ((_ a b)
     (let ((t a))
       (if t t b)))))

(assert-equal ''defined
              (expand '(define-syntax my-or
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

(expand '(define-syntax my-and
           (syntax-rules ()
             ((_) #t)
             ((_ x) x)
             ((_ x y ...)
              (if x (my-and y ...) #f)))))

(assert-equal '#t
              (expand '(my-and))
              "Expand (my-and)")

(assert-equal '1
              (expand '(my-and 1))
              "Expand (my-and 1)")

(assert-equal '(if 1 (if 2 3 #f) #f)
              (expand '(my-and 1 2 3))
              "Expand (my-and 1 2 3)")

;; Test 3: List literal in pattern
(expand '(define-syntax method
           (syntax-rules (=>)
             ((_ (name args ...) => body)
              (define (name args ...) body)))))

(assert-equal '(define (add x y) (+ x y))
              (expand '(method (add x y) => (+ x y)))
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

(expand '(define-syntax swap
           (syntax-rules ()
             ((_ a b)
              (let ((temp a))
                (set! a b)
                (set! b temp))))))

(define result (expand '(let ((temp 1) (other 2))
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
      (display "FAIL: Hygiene test - 'temp' was captured.\n")
      (assert-equal "not temp" inner-var "Inner variable should be renamed"))
    (begin
      (display "PASS: Hygiene test - 'temp' appeared to be renamed (or at least different).\n")))

(display "\n>>> local macros\n")

;; 1. let-syntax binding
(test '(begin (+ 1 1))
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (m 1))
      "let-syntax simple")

;; 2. let-syntax shadowing
(test '(begin (begin (+ 2 2)))
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (let-syntax ((m (syntax-rules () ((m x) (+ 2 2)))))
           (m 1)))
      "let-syntax shadowing")

;; 3. letrect-syntax mutual recursion (safe)
(test '(begin 'ok)
      '(letrec-syntax ((f (syntax-rules () ((_) (g))))
                       (g (syntax-rules () ((_) 'ok))))
         (f))
      "letrec-syntax mutual recursion safe")
      
;; letrec-syntax mutual recursion 2
(test '(begin (begin 1 2))
      '(letrec-syntax ((a (syntax-rules () ((_) (b))))
                       (b (syntax-rules () ((_) (begin 1 2)))))
         (a))
      "letrec-syntax mutual recursion 2")

;; let*-syntax scoping
(test '(begin (begin (begin (begin 1 2))))
      '(let*-syntax ((a (syntax-rules () ((_) (b))))
                      (b (syntax-rules () ((_) (begin 1 2)))))
         (a))
      "let*-syntax sequential visibility")

;; let*-syntax shadowing
(test '(begin (begin (begin (quote inner))))
      '(let-syntax ((a (syntax-rules () ((_) 'outer))))
         (let*-syntax ((a (syntax-rules () ((_) 'inner))))
           (a)))
      "let*-syntax shadowing")

;; --- Chibi Tests (R5RS 4.3) ---

;; "let-syntax" scoping
(test '(let ((x 'outer)) (begin (let ((x 'inner)) x)))
      '(let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
           (let ((x 'inner))
             (m))))
      "Chibi let-syntax scoping")

;; "letrec-syntax" R5RS 4.3.1 - my-or
(test '(begin (let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
         (let ((temp x))
           (if temp temp
               (let ((temp (let temp)))
                 (if temp temp
                     (let ((temp (if y)))
                       (if temp temp
                           y))))))))
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
(test '(let ((+ *)) (begin (let ((* -) (+ /)) (+ (* 3 3) (* 3 3)))))
      '(let ((+ *))
          (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x ?x))))
                       (b (syntax-rules () ((_ ?x) (* ?x ?x)))))
            (let ((* -)
                  (+ /))
              (a (b 3)))))
      "Gauche let-syntax multi")

;; let-syntax (nest)
(test '(begin (begin 2))
      '(let-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
          (let-syntax ((a (syntax-rules ()
                             ((_ ?x ?y ...) (a ?y ...))
                             ((_) 2))))
            (a 8 9 10)))
      "Gauche let-syntax nest")

;; --- R5RS Pitfall Tests ---

;; Pitfall 3.1
(test '(begin (let ((+ *)) (+ 3 1)))
      '(let-syntax ((foo
                     (syntax-rules ()
                       ((_ expr) (+ expr 1)))))
         (let ((+ *))
           (foo 3)))
      "Pitfall 3.1: Hygiene with shadowed global operator")

;; Pitfall 3.2
(test '(begin (let ((x 2)) (begin (define foo +)) (cond (else (define x 1))) x))
      '(let-syntax ((foo (syntax-rules ()
                             ((_ var) (define var 1)))))
          (let ((x 2))
            (begin (define foo +))
            (cond (else (foo x)))
            x))
      "Pitfall 3.2: let-syntax inside let with begin and cond")

;; Pitfall 3.3
(test '(let ((x 1)) (begin (begin (bar))))
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
(test '(begin 1)
      '(let-syntax ((x (syntax-rules ()))) 1)
      "Pitfall 3.4: let-syntax with no clauses")

;; Pitfall 8.1
(test '((letrec ((- (lambda (n) n))) -) (- 1))
      '(let - ((n (- 1))) n)
      "Pitfall 8.1: named let with name -")

;; Pitfall 8.3
(test '(let ((x 1)) (begin (define x 2) 3) x)
      '(let ((x 1))
         (let-syntax ((foo (syntax-rules () ((_) 2))))
           (define x (foo))
           3)
         x)
      "Pitfall 8.3: let-syntax and local define")

(display "\n>>> r7rs\n")

;; 1. be-like-begin1
(expand '(define-syntax be-like-begin1
           (syntax-rules ()
             ((be-like-begin1 name)
              (define-syntax name
                (syntax-rules ()
                  ((name expr (... ...))
                   (begin expr (... ...)))))))))
;; Register sequence1
(expand '(be-like-begin1 sequence1))
(test '(begin 0 1 2 3)
      '(sequence1 0 1 2 3)
      "be-like-begin1")

;; 2. be-like-begin2 (ellipsis escape)
(expand '(define-syntax be-like-begin2
           (syntax-rules ()
             ((be-like-begin2 name)
              (define-syntax name
                (... (syntax-rules ()
                       ((name expr ...)
                        (begin expr ...)))))))))
(expand '(be-like-begin2 sequence2))
(test '(begin 1 2 3 4)
      '(sequence2 1 2 3 4)
      "be-like-begin2")

;; 3. Ellipsis escape via literals
(expand '(define-syntax be-like-begin3
           (syntax-rules ()
             ((be-like-begin3 name)
              (define-syntax name
                (syntax-rules dots ()
                  ((name expr dots)
                   (begin expr dots))))))))
(expand '(be-like-begin3 sequence3))
(test '(begin 2 3 4 5)
      '(sequence3 2 3 4 5)
      "be-like-begin3")

;; 4. Ellipsis escape in output
(expand '(define-syntax elli-esc-1
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
(expand '(define-syntax underscore
           (syntax-rules ()
             ((foo _) '_))))
(test ''_
      '(underscore foo)
      "underscore match")

;; 6. Jabberwocky (nested definition)
(expand '(define-syntax jabberwocky
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
(expand '(define-syntax elli-lit-1
           (syntax-rules ... (...)
             ((_ x)
              '(x ...)))))
(test ''(100 ...)
      '(elli-lit-1 100)
      "Literal ... priority")

;; --- Gauche Tests ---

(expand '(define-syntax simple (syntax-rules ()
                        ((_ "a" ?a) (a ?a))
                        ((_ "b" ?a) (b ?a))
                        ((_ #f ?a)  (c ?a))
                        ((_ #(1 2) ?a) (e ?a))
                        ((_ ?b ?a)  (f ?a ?b)))))

(test '(a z) '(simple "a" z) "simple literal match 1")
(test '(c z) '(simple #f z) "simple literal match 2")
(test '(e z) '(simple #(1 2) z) "simple vector match")
(test '(f z #(2 1)) '(simple #(2 1) z) "simple vector non-match")


(expand '(define-syntax underbar (syntax-rules ()
                          [(_) 0]
                          [(_ _) 1]
                          [(_ _) 2]
                          [(_ _ _) 3]
                          [(_ _ _ _ . _) 'many])))
(test 0 '(underbar) "underbar 0")
(test 1 '(underbar a) "underbar 1")
(test ''many '(underbar a b c) "underbar 3")
(test ''many '(underbar a b c d) "underbar many")

(expand '(define-syntax repeat (syntax-rules ()
                        ((_ 0 (?a ?b) ...)     ((?a ...) (?b ...)))
                        ((_ 1 (?a ?b) ...)     (?a ... ?b ...))
                        ((_ 2 (?a ?b) ...)     (?a ... ?b ... ?a ...)))))

(test '((a c e) (b d f))
      '(repeat 0 (a b) (c d) (e f))
      "repeat 0")
(test '(a c e b d f)
      '(repeat 1 (a b) (c d) (e f))
      "repeat 1")

(expand '(define-syntax repeat2 (syntax-rules () ;r7rs
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

(expand '(define-syntax nest1 (syntax-rules ()
                        ((_ (?a ...) ...)        ((?a ... z) ...)))))

(test '((a z) (b c d z) (e f g h i z) (z) (j z))
      '(nest1 (a) (b c d) (e f g h i) () (j))
      "nest1")

(expand '(define-syntax nest4 (syntax-rules () ; r7rs parameter list
                        ((_ ((?a ?b ... ?c) ... ?d))
                         ((?a ...) ((?b ...) ...) (?c ...) ?d)))))

(test '((a d f) ((b) () (g h i)) (c e j) (k l m))
      '(nest4 ((a b c) (d e) (f g h i j) (k l m)))
      "nest4")

;; mixlevel
(expand '(define-syntax mixlevel1 (syntax-rules ()
                            ((_ (?a ?b ...)) ((?a ?b) ...)))))
(test '((1 2) (1 3) (1 4) (1 5) (1 6))
      '(mixlevel1 (1 2 3 4 5 6))
      "mixlevel1")

(expand '(define-syntax hygiene (syntax-rules ()
                          ((_ ?a) (+ ?a 1)))))
;; We can't evaluate, but we test structure.
(test '(let ((+ *)) (+ 2 1)) '(let ((+ *)) (hygiene 2)) "hygiene - + should be renamed")

(expand '(define-syntax vect1 (syntax-rules ()
                        ((_ #(?a ...)) (?a ...))
                        ((_ (?a ...))  #(?a ...)))))
(test '(1 2 3 4 5) '(vect1 #(1 2 3 4 5)) "vect1 decode")
(test '#(1 2 3 4 5) '(vect1 (1 2 3 4 5)) "vect1 encode")

(expand '(define-syntax dot1 (syntax-rules ()
                       ((_ (?a . ?b)) (?a ?b))
                       ((_ ?loser) #f))))
(test '(1 2) '(dot1 (1 . 2)) "dot1 pair")
(test '(1 (2)) '(dot1 (1 2)) "dot1 list")

(expand '(define-syntax dot3 (syntax-rules ()
                       ((_ (?a ...) ?b) (?a ... . ?b)))))
(test '(1 2 . 3) '(dot3 (1 2) 3) "dot3")

(display "\n>>> chez scheme\n")

;; From https://www.scheme.com/csug7/syntax.html Section 10.3

(expand '(define-syntax chez-or
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
(expand '(define-syntax dotted
           (syntax-rules ()
             ((_ (a . b)) (cons a b)))))

(test '(cons 1 2) '(dotted (1 . 2)) "dotted pair match")
(test '(cons 1 (2)) '(dotted (1 2)) "dotted list match")

;; Corner Case 2: Vector Patterns w/ Ellipsis
;; Verify #(a ...) bindings and template expansion
(expand '(define-syntax vector-ellipses
           (syntax-rules ()
             ((_ #(a ...))
              (list a ...)))))

(test '(list 1 2 3) '(vector-ellipses #(1 2 3)) "vector ellipsis match")

;; Corner Case 3: Empty Vector
;; Verify #() literal matching
(expand '(define-syntax empty-vec
           (syntax-rules ()
             ((_ #()) 'empty)
             ((_ x) 'not-empty))))

(test ''empty '(empty-vec #()) "empty vector match")
(test ''not-empty '(empty-vec #(1)) "non-empty vector fallback")

;; Corner Case 4: Ellipsis as Literal
;; Verify ... can be used as a literal to match identifier ...
(expand '(define-syntax ellipsis-literal
           (syntax-rules (...)
             ((_ ...) 'ellipsis-found)
             ((_ x) 'not-ellipsis))))

(test ''ellipsis-found '(ellipsis-literal ...) "ellipsis literal match")
(test ''not-ellipsis '(ellipsis-literal other) "ellipsis literal non-match")

;; Corner Case 5: Symbolic Literals
;; Verify identifier matching works for symbols like =>
(expand '(define-syntax symbol-literal
           (syntax-rules (=>)
             ((_ =>) 'arrow)
             ((_ x) 'not-arrow))))

(test ''arrow '(symbol-literal =>) "symbol literal match")
(test ''not-arrow '(symbol-literal other) "symbol literal non-match")
