;; test_syntax_case_integrated.scm
(load "./macroexpand.scm")

(define (test name output expected)
  (display name)
  (if (equal? output expected)
      (display " ... PASS\n")
      (begin
        (display " ... FAIL\n")
        (display "  Expected: ") (write expected) (newline)
        (display "  Got:      ") (write output) (newline))))

;; Define a macro using syntax-case
(macroexpand 
 '(define-syntax reverse-params
    (lambda (x)
      (syntax-case x ()
        ((_ a b c) (syntax (list c b a)))))))

(test "integrated-syntax-case"
      (macroexpand '(reverse-params 1 2 3) 'strip)
      '(list 3 2 1))

;; R6RS 'or' macro
(macroexpand
 '(define-syntax r6rs-or
    (lambda (x)
      (syntax-case x ()
        [(_) (syntax #f)]
        [(_ e) (syntax e)]
        [(_ e1 e2 e3 ...)
         (syntax (let ([t e1])
                   (if t t (r6rs-or e2 e3 ...))))]))))

(test "r6rs-or-zero"
      (macroexpand '(r6rs-or) 'strip)
      '#f)

(test "r6rs-or-one"
      (macroexpand '(r6rs-or 1) 'strip)
      '1)

(test "r6rs-or-many"
      (macroexpand '(r6rs-or 1 2 3) 'strip)
      '(let ((t 1)) (if t t (let ((t 2)) (if t t 3)))))

;; R6RS p.car (identifier macro)
(macroexpand
 '(define-syntax p.car
    (lambda (x)
      (syntax-case x ()
        [(_ . rest) (syntax ((car p) . rest))]
        [_ (syntax (car p))]))))

(test "identifier-macro-call"
      (macroexpand '(p.car 1 2) 'strip)
      '((car p) 1 2))

(test "identifier-macro-ref"
      (macroexpand 'p.car 'strip)
      '(car p))

;; R6RS 'let' with duplicate detection
(define (unique-ids? ls)
  (or (null? ls)
      (and (let loop ((x (car ls)) (rest (cdr ls)))
             (or (null? rest)
                 (and (not (bound-identifier=? x (car rest)))
                      (loop x (cdr rest)))))
           (unique-ids? (cdr ls)))))

(macroexpand
 '(define-syntax r6rs-let
    (lambda (x)
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (if (unique-ids? (syntax (i ...)))
             (syntax ((lambda (i ...) e1 e2 ...) v ...))
             (error "duplicate identifiers")))))))

(test "r6rs-let-basic"
      (macroexpand '(r6rs-let ((x 1) (y 2)) (+ x y)) 'strip)
      '((lambda (x y) (+ x y)) 1 2))

;; R6RS 'rec' macro
(macroexpand
 '(define-syntax rec
    (lambda (x)
      (syntax-case x ()
        [(_ x e)
         (identifier? (syntax x))
         (syntax (letrec ([x e]) x))]))))

(test "r6rs-rec-fact"
      (macroexpand 
       '(map (rec fact
                  (lambda (n)
                    (if (= n 0)                 
                        1
                        (* n (fact (- n 1))))))
             '(1 2 3 4 5))
       'strip)
      '(map (letrec* ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) fact) (quote (1 2 3 4 5))))

;; Anaphoric IF (aif)
(macroexpand
 '(define-syntax aif
    (lambda (x)
      (syntax-case x ()
        ((_ test true false)
         (with-syntax ((it (datum->syntax (syntax test) (string->symbol "it"))))
           (syntax (let ((it test))
                     (if it true false)))))))))

(test "aif-test"
      (macroexpand '(aif (assoc 'a '((a . 1))) (cdr it) #f) 'strip)
      '(let ((it (assoc (quote a) (quote ((a . 1)))))) (if it (cdr it) #f)))

;; Simple define-struct
(macroexpand
 '(define-syntax define-struct
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
                     (define (name? obj) (and (pair? obj) (eq? (car obj) (quote name))))))))))))

(test "define-struct-expansion"
      (macroexpand '(define-struct point (x y)) 'strip)
      '(begin
         (define (make-point x y) (list (quote point) x y))
         (define (point? obj) (and (pair? obj) (eq? (car obj) (quote point))))))

;; Stringify macro for constant testing
(macroexpand
 '(define-syntax stringify
    (lambda (x)
      (syntax-case x ()
        ((_ n)
         (syntax (number->string n)))))))

(test "stringify-constant"
      (macroexpand '(stringify 100) 'strip)
      '(number->string 100))

;; Stringify macro for symbol testing
(macroexpand
 '(define-syntax stringify
    (lambda (x)
      (syntax-case x ()
        ((_ n)
         (syntax (symbol->string n)))))))

(test "stringify-symbl"
      (macroexpand '(stringify 'hoge) 'strip)
      '(symbol->string 'hoge))

;; with-syntax with ellipsis
(macroexpand
 '(define-syntax list-to-vars
    (lambda (x)
      (syntax-case x ()
        ((_ (vals ...))
         (with-syntax (((v ...) (syntax (vals ...))))
           (syntax (list v ...))))))))

(test "with-syntax-ellipsis"
      (macroexpand '(list-to-vars (1 2 3)) 'strip)
      '(list 1 2 3))

;; Nested with-syntax dependencies
(macroexpand
 '(define-syntax nested-with
    (lambda (x)
      (syntax-case x ()
        ((_ val)
         (with-syntax ((a (syntax val)))
           (with-syntax ((b (datum->syntax (syntax a) 'inner-sym)))
             (syntax (list a b)))))))))

(test "with-syntax-nested"
      (macroexpand '(nested-with 99) 'strip)
      '(list 99 inner-sym))

;; quasisyntax test
(macroexpand
 '(define-syntax test-quasisyntax
    (lambda (x)
      (syntax-case x ()
        ((_ val)
         (quasisyntax (list (unsyntax (syntax val)) (unsyntax (+ (syntax->datum (syntax val)) 1)))))))))

(test "quasisyntax-basic"
      (macroexpand '(test-quasisyntax 10) 'strip)
      '(list 10 11))

;; quasisyntax with unsyntax-splicing
(macroexpand
 '(define-syntax test-unsyntax-splicing
    (lambda (x)
      (syntax-case x ()
        ((_ vals)
         (quasisyntax (list (unsyntax-splicing (syntax->datum (syntax vals))))))))))

(test "quasisyntax-splicing"
      (macroexpand '(test-unsyntax-splicing (1 2 3)) 'strip)
      '(list 1 2 3))

;; Nested quasisyntax
(macroexpand
 '(define-syntax nested-quasisyntax
    (lambda (x)
      (syntax-case x ()
        ((_ val)
         (quasisyntax (list (unsyntax (quasisyntax (list (unsyntax (syntax val))))))))))))

(test "quasisyntax-nested"
      (macroexpand '(nested-quasisyntax 42) 'strip)
      '(list (list 42)))

(display "Done.\n")
