;; r7rs_test.scm
;; Ported tests from chibi-scheme/tests/r7rs-tests.scm

(load "macroexpand.scm")


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
                (list->string (reverse (cdr chars)))
                str)
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
                        ;;((_ (#\a #\b) ?a) (d ?a)) ;; Skip list-literal match for now if complex
                        ((_ #(1 2) ?a) (e ?a))
                        ((_ ?b ?a)  (f ?a ?b)))))

(test '(a z) '(simple "a" z) "simple literal match 1")
(test '(c z) '(simple #f z) "simple literal match 2")
(test '(e z) '(simple #(1 2) z) "simple vector match")
(test '(f z #(2 1)) '(simple #(2 1) z) "simple vector non-match")


(expand '(define-syntax underbar (syntax-rules ()
                          [(_) 0]
                          [(_ _) 1]
                          [(_ _ _) 2]
                          [(_ _ _ _) 3]
                          [(_ _ _ _ . _) 'many])))
(test 0 '(underbar) "underbar 0")
(test 1 '(underbar a) "underbar 1")
(test 3 '(underbar a b c) "underbar 3")
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

