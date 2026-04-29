;; ==== Combined Syntax Tests ====
(define *pass-count* 0)
(define *fail-count* 0)

(copy-environment-variables! (system-environment) (current-environment) '(make-syntax-object strip-renames expand-syntax-case))

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

(define (test-eq name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

(define (test-eval name expr expected)
  (let ((result (core-eval (macroexpand expr) (current-environment))))
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
  (let ((result (core-eval (macroexpand expr 'strip) (current-environment))))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

(define (test-eval-sc expected expr msg)
  (test-eval msg expr expected))

(define (test-eval-strip-sc expected expr msg)
  (test-eval-strip msg expr expected))

;; =============================================================================
;; Section 1: syntax-rules Basics
;; =============================================================================
(display "\n>>> Section 1: syntax-rules Basics\n")

(test "Register my-or"
      '(define-syntax my-or
         (syntax-rules ()
           ((_ a b)
            (let ((t a))
              (if t t b)))))
      (unspecified))

(test "Expand my-or" '(my-or #t #f) '(let ((t #t)) (if t t #f)))

(macroexpand '(define-syntax my-and
                (syntax-rules ()
                  ((_) #t)
                  ((_ x) x)
                  ((_ x y ...)
                   (if x (my-and y ...) #f)))))

(test "Expand (my-and)" '(my-and) '#t)
(test "Expand (my-and 1)" '(my-and 1) '1)
(test "Expand (my-and 1 2 3)" '(my-and 1 2 3) '(if 1 (if 2 3 #f) #f))

(macroexpand '(define-syntax method
                (syntax-rules (=>)
                  ((_ (name args ...) => body)
                   (define (name args ...) body)))))

(test "Expand method with literal =>"
      '(method (add x y) => (+ x y))
      '(define add (lambda (x y) (+ x y))))

(test "Lambda identity" '(lambda (x) x) '(lambda (x) x))

;; =============================================================================
;; Section 2: Hygiene & Variable Capture
;; =============================================================================
(display "\n>>> Section 2: Hygiene\n")

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

(define inner-let (caddr result))
(define inner-binding (car (cadr inner-let)))
(define inner-var (car inner-binding))

(if (eq? inner-var 'temp)
    (begin
      (set! *fail-count* (+ *fail-count* 1))
      (display "FAIL: Hygiene test - 'temp' was captured.\n"))
    (begin
      (set! *pass-count* (+ *pass-count* 1))
      (display "PASS: Hygiene test - 'temp' appeared to be renamed (or at least different).\n")))

;; =============================================================================
;; Section 3: Local Macros
;; =============================================================================
(display "\n>>> Section 3: Local Macros\n")

(test "let-syntax simple"
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (m 1))
      '(+ 1 1))

(test "let-syntax shadowing"
      '(let-syntax ((m (syntax-rules () ((m x) (+ x x)))))
         (let-syntax ((m (syntax-rules () ((m x) (+ 2 2)))))
           (m 1)))
      '(+ 2 2))

(test "letrec-syntax mutual recursion safe"
      '(letrec-syntax ((f (syntax-rules () ((_) (g))))
                       (g (syntax-rules () ((_) 'ok))))
         (f))
      ''ok)

(test "letrec-syntax mutual recursion 2"
      '(letrec-syntax ((a (syntax-rules () ((_) (b))))
                       (b (syntax-rules () ((_) (begin 1 2)))))
         (a))
      '(begin 1 2))

(test "let*-syntax sequential visibility"
      '(let*-syntax ((a (syntax-rules () ((_) (begin 1 2))))
                     (b (syntax-rules () ((_) (a)))))
         (b))
      '(begin 1 2))

(test "let*-syntax shadowing"
      '(let-syntax ((a (syntax-rules () ((_) 'outer))))
         (let*-syntax ((a (syntax-rules () ((_) 'inner))))
           (a)))
      ''inner)

(test "Chibi let-syntax scoping"
      '(let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
           (let ((x 'inner))
             (m))))
      '(let ((x 'outer))
         (let ((x 'inner)) x)))

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

(test "Gauche let-syntax multi"
      '(let ((+ *))
         (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x ?x))))
                      (b (syntax-rules () ((_ ?x) (* ?x ?x)))))
           (let ((* -)
                 (+ /))
             (a (b 3)))))
      '(let ((+ *)) (let ((* -) (+ /)) (+ (* 3 3) (* 3 3)))))

(test "Gauche let-syntax nest"
      '(let-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
         (let-syntax ((a (syntax-rules ()
                           ((_ ?x ?y ...) (a ?y ...))
                           ((_) 2))))
           (a 8 9 10)))
      '(+ 9 10))

(test-eval "Pitfall 3.1: Hygiene with shadowed global operator"
           '(let-syntax ((foo
                          (syntax-rules ()
                            ((_ expr) (+ expr 1)))))
              (let ((+ *))
                (foo 3)))
           4)

(test-eval "Pitfall 3.2: let-syntax inside let with begin and cond"
           '(let-syntax ((foo (syntax-rules ()
                                ((_ var) (define var 1)))))
              (let ((x 2))
                (begin (define foo +))
                (cond (else (foo x)))
                x))
           2)

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

(test-eval "Pitfall 3.4: let-syntax with no clauses"
           '(let-syntax ((x (syntax-rules ()))) 1)
           1)

(test-eval "Pitfall 8.1: named let with name -"
           '(let - ((n (- 1))) n)
           -1)

(test-eval-strip "Pitfall 8.3: let-syntax and local define"
                 '(let ((x 1))
                    (let-syntax ((foo (syntax-rules () ((_) 2))))
                      (define x (foo))
                      3)
                    x)
                 2)

;; =============================================================================
;; Section 4: R7RS Syntax Features & Corner Cases
;; =============================================================================
(display "\n>>> Section 4: R7RS & Corner Cases\n")

(macroexpand '(define-syntax be-like-begin1
                (syntax-rules ()
                  ((be-like-begin1 name)
                   (define-syntax name
                     (syntax-rules ()
                       ((name expr (... ...))
                        (begin expr (... ...)))))))))

(macroexpand '(be-like-begin1 sequence1))
(test "be-like-begin1" '(sequence1 0 1 2 3) '(begin 0 1 2 3))

(macroexpand '(define-syntax be-like-begin2
                (syntax-rules ()
                  ((be-like-begin2 name)
                   (define-syntax name
                     (... (syntax-rules ()
                            ((name expr ...)
                             (begin expr ...)))))))))

(macroexpand '(be-like-begin2 sequence2))
(test "be-like-begin2" '(sequence2 1 2 3 4) '(begin 1 2 3 4))

(macroexpand '(define-syntax be-like-begin3
                (syntax-rules ()
                  ((be-like-begin3 name)
                   (define-syntax name
                     (syntax-rules dots ()
                       ((name expr dots)
                        (begin expr dots))))))))

(macroexpand '(be-like-begin3 sequence3))
(test "be-like-begin3" '(sequence3 2 3 4 5) '(begin 2 3 4 5))

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

(macroexpand '(define-syntax underscore
                (syntax-rules ()
                  ((foo _) '_))))
(test "underscore match" '(underscore foo) ''_)

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
      `(begin (define march-hare 42) ,(unspecified)))

(macroexpand '(define-syntax elli-lit-1
                (syntax-rules ... (...)
                  ((_ x)
                   '(x ...)))))
(test "Literal ... priority" '(elli-lit-1 100) ''(100 ...))

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

(macroexpand '(define-syntax repeat2 (syntax-rules ()
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

(macroexpand '(define-syntax nest4 (syntax-rules ()
                                     ((_ ((?a ?b ... ?c) ... ?d))
                                      ((?a ...) ((?b ...) ...) (?c ...) ?d)))))

(test "nest4"
      '(nest4 ((a b c) (d e) (f g h i j) (k l m)))
      '((a d f) ((b) () (g h i)) (c e j) (k l m)))

(macroexpand '(define-syntax mixlevel1 (syntax-rules ()
                                         ((_ (?a ?b ...)) ((?a ?b) ...)))))
(test "mixlevel1" '(mixlevel1 (1 2 3 4 5 6)) '((1 2) (1 3) (1 4) (1 5) (1 6)))

(macroexpand '(define-syntax hygiene (syntax-rules ()
                                       ((_ ?a) (+ ?a 1)))))
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

(macroexpand '(define-syntax chez-or
                (syntax-rules ()
                  ((_) #f)
                  ((_ e) e)
                  ((_ e1 e2 e3 ...)
                   (let ((t e1))
                     (if t t (chez-or e2 e3 ...)))))))

(test "Chez Scheme hygiene test: or, if, t shadowing"
      '(let ((if #f))
         (let ((t 'okay))
           (chez-or if t)))
      '(let ((if #f))
         (let ((t 'okay))
           (let ((t if))
             (if t t t)))))

(macroexpand '(define-syntax dotted
                (syntax-rules ()
                  ((_ (a . b)) (cons a b)))))

(test "dotted pair match" '(dotted (1 . 2)) '(cons 1 2))
(test "dotted list match" '(dotted (1 2)) '(cons 1 (2)))

(macroexpand '(define-syntax vector-ellipses
                (syntax-rules ()
                  ((_ #(a ...))
                   (list a ...)))))

(test "vector ellipsis match" '(vector-ellipses #(1 2 3)) '(list 1 2 3))

(macroexpand '(define-syntax empty-vec
                (syntax-rules ()
                  ((_ #()) 'empty)
                  ((_ x) 'not-empty))))

(test "empty vector match" '(empty-vec #()) ''empty)
(test "non-empty vector fallback" '(empty-vec #(1)) ''not-empty)

(macroexpand '(define-syntax ellipsis-literal
                (syntax-rules (...)
                  ((_ ...) 'ellipsis-found)
                  ((_ x) 'not-ellipsis))))

(test "ellipsis literal match" '(ellipsis-literal ...) ''ellipsis-found)
(test "ellipsis literal non-match" '(ellipsis-literal other) ''not-ellipsis)

(macroexpand '(define-syntax symbol-literal
                (syntax-rules (=>)
                  ((_ =>) 'arrow)
                  ((_ x) 'not-arrow))))

(test "symbol literal match" '(symbol-literal =>) ''arrow)
(test "symbol literal non-match" '(symbol-literal other) ''not-arrow)

(test "User requested: letrec-syntax scoping case"
      '(let ((f (lambda (x) (+ x 1))))
         (letrec-syntax ((f (syntax-rules () ((_ x) x)))
                         (g (syntax-rules () ((_ x) (f x)))))
           (list (f 1) (g 1))))
      '(let ((f (lambda (x) (+ x 1))))
         (list 1 1)))

(test "User requested: let-syntax scoping case"
      '(let ((f (lambda (x) (+ x 1))))
         (let-syntax ((f (syntax-rules () ((_ x) x)))
                      (g (syntax-rules () ((_ x) (f x)))))
           (list (f 1) (g 1))))
      '(let ((f (lambda (x) (+ x 1))))
         (list 1 (f 1))))

;; Missing Test: Vector Tail Pattern
(define-syntax vect-tail
  (syntax-rules ()
    ((_ #(?a ... ?b)) (list (list ?a ...) ?b))))

(test "vector tail pattern"
      '(vect-tail #(1 2 3 4))
      '(list (list 1 2 3) 4))

;; =============================================================================
;; Section 5: Core Forms
;; =============================================================================
(display "\n>>> Section 5: Core Forms\n")

(test "let* sequential binding"
      '(let* ((x 1) (y (+ x 1))) (+ x y))
      '(let ((x 1)) (let ((y (+ x 1))) (+ x y))))

(test "letrec* sequential recursive binding"
      '(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))
      '(letrec* ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
         (f 5)))

(test "lambda internal define simple"
      '(lambda (x) (define y 1) (+ x y))
      '(lambda (x) (letrec* ((y 1)) (+ x y))))

(test "lambda internal define function syntax"
      '(lambda (x) (define (f n) (* n 2)) (f x))
      '(lambda (x) (letrec* ((f (lambda (n) (* n 2)))) (f x))))

(test "lambda multiple internal defines"
      '(lambda (x) (define a 1) (define b 2) (+ x a b))
      '(lambda (x) (letrec* ((a 1) (b 2)) (+ x a b))))

(test "let internal define"
      '(let ((x 1)) (define y 2) (+ x y))
      '(let ((x 1)) (letrec* ((y 2)) (+ x y))))

(test "let* internal define"
      '(let* ((a 1) (b 2)) (define c 3) (+ a b c))
      '(let ((a 1)) (let ((b 2)) (letrec* ((c 3)) (+ a b c)))))

(test "letrec internal define"
      '(letrec ((f (lambda (x) x))) (define (g y) y) (f (g 1)))
      '(letrec* ((f (lambda (x) x)) (g (lambda (y) y))) (f (g 1))))

(test "letrec* internal define merged"
      '(letrec* ((a 1) (b 2)) (define c 3) (+ a b c))
      '(letrec* ((a 1) (b 2) (c 3)) (+ a b c)))

(test "let internal define 2"
      '(let ((x 1)) (define y 2) (define z 3) (+ x y z))
      '(let ((x 1)) (letrec* ((y 2) (z 3)) (+ x y z))))

(test "lambda without internal define unchanged"
      '(lambda (x) (+ x 1))
      '(lambda (x) (+ x 1)))

;; =============================================================================
;; Section 6: Advanced Syntax
;; =============================================================================
(display "\n>>> Section 6: Advanced Syntax\n")

(macroexpand '(define-syntax lambda*-helper
                (syntax-rules ()
                  [(_ (h . t) (id ...) body)
                   (lambda*-helper t (id ... h) body)]
                  [(_ rest (id ...) (b0 b1 ...))
                   (lambda args
                     (let-ids args (id ... rest) b0 b1 ...))])))

(macroexpand '(define-syntax let-ids
                (syntax-rules ()
                  [(_ ls (last) b0 b1 ...)
                   (let ([last ls]) b0 b1 ...)]
                  [(_ ls (id0 id1 ...) b0 b1 ...)
                   (let ([id0 (car ls)]
                         [next (cdr ls)])
                     (let-ids next (id1 ...) b0 b1 ...))])))

(macroexpand '(define-syntax lambda*
                (syntax-rules ()
                  [(_ (id ...) b0 b1 ...)
                   (lambda (id ...) b0 b1 ...)]
                  [(_ (h . t) b0 b1 ...)
                   (lambda*-helper (h . t) () (b0 b1 ...))]
                  [(_ rest b0 b1 ...)
                   (lambda rest b0 b1 ...)])))

(test-eval "lambda* proper list (2 args)"
           '((lambda* (x y) (list x y)) 1 2)
           '(1 2))

(test-eval "lambda* standard rest arg"
           '((lambda* args args) 1 2 3)
           '(1 2 3))

(test-eval "lambda* dotted list (1 fixed)"
           '((lambda* (x . y) (list x y)) 1 2 3)
           '(1 (2 3)))

(test-eval "lambda* dotted list (2 fixed)"
           '((lambda* (x y . z) (list x y z)) 1 2 3 4)
           '(1 2 (3 4)))

(test-eval "lambda* dotted list (rest empty)"
           '((lambda* (x . y) (list x y)) 1)
           '(1 ()))

(macroexpand '(define-syntax ??!apply
  (syntax-rules (??!lambda)
    ((_ (??!lambda (bound-var . other-bound-vars) body) oval . other-ovals)
     (letrec-syntax ((subs
                       (syntax-rules (??! bound-var ??!lambda)
                         ((_ val k (??! bound-var)) (appl k val))
                         ((_ val k (??!lambda bvars int-body))
                          (subs-in-lambda val bvars (k bvars) int-body))
                         ((_ val k (x)) (subs val (recon-pair val k ()) x))
                         ((_ val k (x . y)) (subs val (subsed-cdr val k x) y))
                         ((_ val k x) (appl k x))))
                     (subsed-cdr
                       (syntax-rules ()
                         ((_ val k x new-y)
                          (subs val (recon-pair val k new-y) x))))
                     (recon-pair
                       (syntax-rules ()
                         ((_ val k new-y new-x) (appl k (new-x . new-y)))))
                     (subs-in-lambda
                       (syntax-rules (bound-var)
                         ((_ val () kp int-body)
                          (subs val (recon-l kp ()) int-body))
                         ((_ val (bound-var . obvars) (k bvars) int-body)
                          (appl k (??!lambda bvars int-body)))
                         ((_ val (obvar . obvars) kp int-body)
                          (subs-in-lambda val obvars kp int-body))))
                     (recon-l
                       (syntax-rules ()
                         ((_ (k bvars) () result)
                          (appl k (??!lambda bvars result)))))
                     (appl
                      (syntax-rules ()
                        ((_ (a b c d) result) (a b c d result))
                        ((_ (a b c) result) (a b c result))))
                     (finish
                       (syntax-rules ()
                         ((_ () () exp) exp)
                         ((_ rem-bvars rem-ovals exps)
                          (??!apply (??!lambda rem-bvars exps) . rem-ovals)))))
       (subs oval (finish other-bound-vars other-ovals) body))))))

(macroexpand '(define-syntax ?cons (syntax-rules () ((_ x y k) (??!apply k (x . y))))))

(test-eval "CPS literal substitution via ??!apply"
  '(let ((result #f))
     (?cons hello world (??!lambda (x) (set! result (quote (??! x)))))
     result)
  '(hello . world))

(macroexpand '(define-syntax ?car (syntax-rules () ((_ (x . y) k) (??!apply k x)))))
(macroexpand '(define-syntax ?cdr (syntax-rules () ((_ (x . y) k) (??!apply k y)))))

(test-eval "CPS nested substitution (car of cons)"
  '(let ((result #f))
     (?cons a b (??!lambda (p) (?car (??! p) (??!lambda (x) (set! result (quote (??! x)))))))
     result)
  'a)

(test-eval "CPS nested substitution (cdr of cons)"
  '(let ((result #f))
     (?cons a b (??!lambda (p) (?cdr (??! p) (??!lambda (x) (set! result (quote (??! x)))))))
     result)
  'b)

(test-eval "letrec-syntax literal match across rename boundary"
  '(letrec-syntax
     ((make-matcher
       (syntax-rules ()
         ((_ tag)
          (letrec-syntax
            ((m (syntax-rules (tag)
                  ((_ tag) 'matched)
                  ((_ other) 'no-match))))
            (m tag))))))
     (make-matcher foo))
  'matched)

(test-eval "letrec-syntax literal non-match across rename boundary"
  '(letrec-syntax
     ((make-matcher
       (syntax-rules ()
         ((_ tag)
          (letrec-syntax
            ((m (syntax-rules (tag)
                  ((_ tag) 'matched)
                  ((_ other) 'no-match))))
            (m bar))))))
     (make-matcher foo))
  'no-match)

;; Preserved from test-mini-stress.scm (unused but kept for safety)
(define-syntax ?null? (syntax-rules () ((_ () k) (??!apply k #t)) ((_ x k) (??!apply k #f))))
(define-syntax ?ifnull? (syntax-rules () ((_ () kt kf) (??!apply kt #t)) ((_ x kt kf) (??!apply kf #f))))

(define ans #f)
(?cons hello world (??!lambda (x) (set! ans (quote (??! x)))))
(test-eq "check output pair" ans '(hello . world))

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
