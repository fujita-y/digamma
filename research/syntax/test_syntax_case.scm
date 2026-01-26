;; test_syntax_case.scm
(load "./macroexpand.scm")

(define (test name output expected)
  (if (equal? output expected)
      (begin (display "PASS: ") (display name) (newline))
      (begin
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;;=============================================================================
;; SECTION 1: Standalone expansion tests
;;=============================================================================
(display "\n>>> standalone\n")

;; Test basic matching
(let* ((input (make-syntax-object '(foo 1 2 3) '()))
       (result (expand-syntax-case input '()
                                   '(((name val ...) #t (syntax (name val ...))))
                                   (interaction-environment))))
  (test "basic-syntax-case" (syntax->datum result) '(foo 1 2 3)))

;; Test ellipsis expansion
(let* ((input (make-syntax-object '(test-let ((x 1) (y 2)) + x y) '()))
       (result (expand-syntax-case input '()
                                   '(((test-let ((var val) ...) body ...) #t (syntax (list (list 'var val) ... 'body ...))))
                                   (interaction-environment))))
  (test "ellipsis-expansion" (syntax->datum result) '(list (list 'x 1) (list 'y 2) '+ 'x 'y)))

;; Test fenders
(let* ((input (make-syntax-object '(foo 1 2 3) '()))
       (result (expand-syntax-case input '()
                                   '(((name val ...) (null? (syntax->datum (syntax (val ...)))) 'empty)
                                     ((name val ...) #t 'not-empty))
                                   (interaction-environment))))
  (test "fender-false" result 'not-empty))

;;=============================================================================
;; SECTION 2: Integrated macro expansion tests
;;=============================================================================
(display "\n>>> integrated\n")

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
 '(define-syntax stringify-sym
    (lambda (x)
      (syntax-case x ()
        ((_ n)
         (syntax (symbol->string n)))))))

(test "stringify-symbl"
      (macroexpand '(stringify-sym 'hoge) 'strip)
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

;; generate-temporaries test
(macroexpand
 '(define-syntax test-gen-temp
    (lambda (x)
      (syntax-case x ()
        ((_ x ...)
         (let ((temps (generate-temporaries (syntax (x ...)))))
           (with-syntax (((t ...) temps))
             (syntax (list t ...)))))))))

(test "generate-temporaries"
      (let ((res (macroexpand '(test-gen-temp a b))))
        (and (list? res) (= (length res) 3) (eq? (car res) 'list)
             (symbol? (cadr res)) (symbol? (caddr res))
             (not (eq? (cadr res) (caddr res)))))
      #t)

;; syntax->datum in macro body
(macroexpand
 '(define-syntax test-syntax-datum
    (lambda (x)
      (syntax-case x ()
        ((_ a b c)
         (let ((lst (syntax->datum (syntax (a b c)))))
           (with-syntax ((res (list->vector lst)))
             (syntax (quote res)))))))))

(test "syntax->datum-in-macro"
      (macroexpand '(test-syntax-datum 1 2 3) 'strip)
      ''#(1 2 3))

;; datum->syntax test
(macroexpand
 '(define-syntax test-datum-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ name val)
         (with-syntax ((new-name (datum->syntax (syntax name)
                                                (string->symbol (string-append "prefix-"
                                                                               (symbol->string (syntax->datum (syntax name))))))))
           (syntax (define new-name val))))))))

(test "datum->syntax-test"
      (macroexpand '(test-datum-syntax foo 42) 'strip)
      '(define prefix-foo 42))

;; tailmatch test (Backtracking ellipsis + Improper list)
(macroexpand
 '(define-syntax tailmatch1
    (lambda (x)
      (syntax-case x ()
        ((_ first ... last . rest)
         (syntax rest))))))

(test "tailmatch-greedy-backtracking1"
      (macroexpand '(tailmatch1 1 2 3 . 4) 'strip)
      '4)

(macroexpand
 '(define-syntax tailmatch2
    (lambda (x)
      (syntax-case x ()
        ((_ first ... . rest)
         (syntax rest))))))

(test "tailmatch2-greedy-backtracking2"
      (macroexpand '(tailmatch2 1 2 3 . 4) 'strip)
      '4)

;; local-definition-test
(macroexpand
 '(define-syntax local-definition-test
    (lambda (x)
      (define list-to-string
        (lambda (lst)
          (if (pair? lst) "ok" (error 'list-to-string "list expected"))))
      (syntax-case x ()
        ((_ ret name (args ...))
         (let ((signature (list-to-string (syntax->datum (syntax (ret args ...))))))
            (with-syntax ((signature signature))
              (syntax signature))))))))

(test "local-definition-test"
      (macroexpand '(local-definition-test int hoge (int int)) 'strip)
      "ok")

(display "\n")
