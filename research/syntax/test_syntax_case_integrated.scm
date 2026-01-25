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

;; Note: testing duplicate failure might be hard as it's an error.
;; We'll just verify it passes for valid input.

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

(display "Done.\n")
