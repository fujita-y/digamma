(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
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

;;;;;

(define-module (core let-values)
  (export let-values)
  (begin
    (define-syntax let-values
      (syntax-rules ()
        ((let-values () body1 body2 ...)
        (let () body1 body2 ...))
        ((let-values (((v ...) expr) rest ...) body1 body2 ...)
        (call-with-values
        (lambda () expr)
        (lambda (v ...)
          (let-values (rest ...) body1 body2 ...))))))))

(define-module (core destructuring-match) 
  (export destructuring-match)
  (import (core let-values))
  (begin

  (define gentemp (lambda () (gensym "tmp")))

  (define ca---r (make-eq-hashtable))
  (define cd---r (make-eq-hashtable))

  (define car+
    (lambda (expr)
      (cond ((and (pair? expr) (hashtable-ref ca---r (car expr) #f))
            => (lambda (a) (cons a (cdr expr))))
            (else (list 'car expr)))))

  (define cdr+
    (lambda (expr)
      (cond ((and (pair? expr) (hashtable-ref cd---r (car expr) #f))
            => (lambda (a) (cons a (cdr expr))))
            (else (list 'cdr expr)))))

  (define duplicates?
    (lambda (lst)
      (and (pair? lst)
          (let loop ((head (car lst)) (rest (cdr lst)))
            (or (memq head rest)
                (and (pair? rest)
                      (loop (car rest) (cdr rest))))))))

  (define ellipsis-pair?
    (lambda (pat)
      (and (pair? pat)
          (symbol? (car pat))
          (pair? (cdr pat))
          (eq? (cadr pat) '...))))

  (define quoted-pair?
    (lambda (pat)
      (and (pair? pat)
          (eq? (car pat) 'quote)
          (pair? (cdr pat))
          (null? (cddr pat)))))

  (define predicate-pair?
    (lambda (pat)
      (and (pair? pat)
          (eq? (car pat) '?)
          (pair? (cdr pat)))))

  (define choose-pred
    (lambda (pat)
      (cond ((or (symbol? pat) (boolean? pat) (null? pat) (char? pat) (fixnum? pat)) 'eq?)
            ((number? pat) 'eqv?)
            (else 'equal?))))

  (define count-non-dotted-pattern
    (lambda (lst)
      (let loop ((lst lst) (n 0))
        (cond ((pair? lst)
              (cond ((predicate-pair? lst) n)
                    (else (loop (cdr lst) (+ n 1)))))
              (else n)))))

  (define memoize-ref
    (lambda (e mem)
      (cond ((assoc e (vector-ref mem 0)) => cdr)
            (else
            (let ((name (gentemp)))
              (begin (vector-set! mem 0 (cons (cons e name) (vector-ref mem 0))) name))))))

  (define compile-match
    (lambda (ren mem pat ref match bind vars)
      (cond ((quoted-pair? pat)
             (values (cons `(,(choose-pred (cadr pat)) ,ref ',(cadr pat)) match) bind vars))
            ((ellipsis-pair? pat)
             (cond ((null? (cddr pat))
                    (if (eq? (car pat) '_)
                        (values (cons `(list? ,ref) match) bind vars)
                        (values (cons `(list? ,ref) match) (cons ref bind) (cons (car pat) vars))))
                   ((or (not (pair? (cddr pat))) (predicate-pair? (cddr pat)))
                    (if (eq? (car pat) '_)
                        (compile-match ren mem (cddr pat) `(last-cdr ,ref) match bind vars)
                        (compile-match ren mem (cddr pat) `(last-cdr ,ref) match (cons `(drop-last-cdr ,ref) bind) (cons (car pat) vars))))
                   ((pair? (cddr pat))
                    (cond ((null? (cdddr pat))
                           (let ((memoize (memoize-ref `(last-pair ,ref) mem)))
                             (if (eq? (car pat) '_)
                                 (compile-match ren mem (cddr pat) memoize
                                               (cons `(and (pair? ,ref) (set! ,memoize (last-pair ,ref))) match)
                                               bind
                                               vars)
                                 (compile-match ren mem (cddr pat) memoize
                                               (cons `(and (pair? ,ref) (set! ,memoize (last-pair ,ref))) match)
                                               (cons `(drop-last-pair ,ref) bind)
                                               (cons (car pat) vars)))))
                           (else
                              (let ((n (- (count-non-dotted-pattern pat) 2)))
                                (let ((memoize (memoize-ref `(last-n-pair ,n ,ref) mem)))
                                  (if (eq? (car pat) '_)
                                      (compile-match ren mem (cddr pat) memoize
                                                    (cons `(and (pair? ,ref) (set! ,memoize (last-n-pair ,n ,ref))) match)
                                                    bind
                                                    vars)
                                      (compile-match ren mem (cddr pat) memoize
                                                    (cons `(and (pair? ,ref) (set! ,memoize (last-n-pair ,n ,ref))) match)
                                                    (cons `(drop-last-n-pair ,n ,ref) bind)
                                                    (cons (car pat) vars))))))))
                   (else (values #f #f #f))))
            ((predicate-pair? pat)
             (let ((renamed (or (hashtable-ref ren (cadr pat) #f) (gentemp))))
               (hashtable-set! ren (cadr pat) renamed)
               (if (null? (cddr pat))
                   (values (cons `(,renamed ,ref) match) bind vars)
                   (compile-match ren mem (caddr pat) ref (cons `(,renamed ,ref) match) bind vars))))
            ((pair? pat)
             (let-values (((match2 bind2 vars2)
                           (compile-match ren mem (car pat) (car+ ref) (cons `(pair? ,ref) match) bind vars)))
               (cond (match2 (compile-match ren mem (cdr pat) (cdr+ ref) match2 bind2 vars2))
                     (else (values #f #f #f)))))
            ((eq? pat '...) (values #f #f #f))
            ((eq? pat '_) (values match bind vars))
            ((symbol? pat) (values match (cons ref bind) (cons pat vars)))
            ((null? pat) (values (cons `(null? ,ref) match) bind vars))
            (else (values (cons `(,(choose-pred pat) ,ref ,pat) match) bind vars)))))

  (define-syntax destructuring-match
      (lambda (x)
        (syntax-case x ()
          ((?_ ?expr ?clauses ...)
          (let ((datum (gentemp))
                (ren (make-eq-hashtable))
                (mem (vector '())))
            (let ((code 
                  (map (lambda (clause)
                         (syntax-case clause ()
                           ((?pat)
                            (let ((pat (syntax->datum (syntax ?pat))))
                              (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                      (match (list #f (reverse match) '() '() (syntax #t) (syntax #t)))
                                      (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                           ((?pat ?body)
                            (let ((pat (syntax->datum (syntax ?pat))))
                              (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                      (match (list #f (reverse match) vars inits (syntax ?body) (syntax #t)))
                                      (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                           ((?pat ?fender ?body)
                            (let ((pat (syntax->datum (syntax ?pat))))
                              (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                      (match (list #f (reverse match) vars inits (syntax ?body) (syntax ?fender)))
                                      (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                           (_ (syntax-violation 'destructuring-match "malformed clause" x clause))))
                         (syntax (?clauses ...)))))
              (let-values (((shares others) (partition car code)))
                (let ((subexprs (map (lambda (e) `(,(car e) (and ,@(cadr e)))) shares))
                      (clauses (map cdr others)))
                  (with-syntax ((?datum (datum->syntax (syntax k) datum))
                                (((?pred-lhs ?pred-rhs) ...)
                                 (map (lambda (a)
                                        (list (datum->syntax (syntax k) (cdr a))
                                              (datum->syntax (syntax ?_) (car a))))
                                        (hashtable->alist ren)))
                                ((?mem ...)
                                 (map (lambda (e) (datum->syntax (syntax k) (cdr e))) (vector-ref mem 0)))
                                ((?subexprs ...)
                                 (map (lambda (e) (datum->syntax (syntax k) e)) subexprs))
                                ((?dispatch ...)
                                 (map (lambda (clause)
                                        (let-values (((tests vars inits body fender) (apply values clause)))
                                          (with-syntax (((?vars ...) (map (lambda (e) (datum->syntax (syntax ?_) e)) vars))
                                                        ((?inits ...) (map (lambda (e) (datum->syntax (syntax k) e)) inits))
                                                        ((?tests ...) (map (lambda (e) (datum->syntax (syntax k) e)) tests))
                                                        (?fender (datum->syntax (syntax ?_) (syntax->datum fender)))
                                                        (?body (datum->syntax (syntax ?_) (syntax->datum body))))
                                            (if (eq? (syntax->datum (syntax ?fender)) #t)
                                                (syntax ((and ?tests ...)
                                                         (let ((?vars ?inits) ...) ?body)))
                                                (syntax ((and ?tests ... (let ((?vars ?inits) ...) ?fender))
                                                         (let ((?vars ?inits) ...) ?body)))))))
                                      clauses)))
                    (syntax (let ((?datum ?expr) (?pred-lhs ?pred-rhs) ... (?mem #f) ...)
                              (let* (?subexprs ...)
                                (cond ?dispatch ... (else #f))))))))))))))

  (for-each (lambda (e) (hashtable-set! ca---r (car e) (cdr e)))
            '((car . caar) (cdr . cadr) (caar . caaar) (cadr . caadr) (cdar . cadar) (cddr . caddr)
              (caaar . caaaar) (caadr . caaadr) (cadar . caadar) (caddr . caaddr) (cdaar . cadaar)
              (cdadr . cadadr) (cddar . caddar) (cdddr . cadddr)))


  (for-each (lambda (e) (hashtable-set! cd---r (car e) (cdr e)))
            '((car . cdar) (cdr . cddr) (caar . cdaar) (cadr . cdadr) (cdar . cddar) (cddr . cdddr)
              (caaar . cdaaar) (caadr . cdaadr) (cadar . cadadar) (caddr . cdaddr) (cdaar . cddaar)
              (cdadr . cddadr) (cddar . cdddar) (cdddr . cddddr)))
))

(import-module (core destructuring-match))

(destructuring-match '(1 2 3) ((a b c) (+ a b c))) ;; 6
(destructuring-match '(quote 1) (('quote e) (list 1)) (_ (list 'nomatch))) ;; (1)

(test "simple match"
      '(destructuring-match '(1 2 3) ((a b c) (+ a b c)))
      6)

(test "quote match"
      '(destructuring-match '(quote 1) (('quote e) (list 1)) (_ (list 'nomatch))) 
      '(1))

(test "ellipsis match"
      '(destructuring-match '(1 2 3 4 5) ((a b ... c) (list a b c)))
      '(1 (2 3 4) 5))

(test "nested lambda call"
      '(destructuring-match '((lambda (x y) (+ x y)) 1 2)
         ((('lambda (vars ...) . body) . args)
          (and (= (length vars) (length args))
               (list vars body args))))
      '((x y) ((+ x y)) (1 2)))

(test "if #t match"
      '(destructuring-match '(#t 1 2)
         ((#t e1 . _) e1))
      1)

(test "if #f match"
      '(destructuring-match '(#f 1 2)
         ((#f _ . e2) e2))
      '(2))

(test "literal match"
      '(destructuring-match '(not x)
         (('not e1) e1))
      'x)

(test "fender match"
      '(destructuring-match '(1 2)
         ((a b) (= a b) 'equal)
         ((a b) 'not-equal))
      'not-equal)

(test "fender match 2"
      '(destructuring-match '(1 1)
         ((a b) (= a b) 'equal)
         ((a b) 'not-equal))
      'equal)

(test "multiple clauses"
      '(destructuring-match '(a b)
         ((x) 'one)
         ((x y) 'two)
         ((x y z) 'three))
      'two)

(test "dotted pattern"
      '(destructuring-match '(1 2 3)
         ((a . b) (list a b)))
      '(1 (2 3)))

(test "underscore match"
      '(destructuring-match '(1 2 3)
         ((_ _ a) a))
      3)

(test "complex list pattern"
      '(destructuring-match '(let ((x 1) (y 2)) (+ x y))
         (('let bindings . body)
          (list bindings body)))
      '(((x 1) (y 2)) ((+ x y))))

(test "predicate match"
      '(destructuring-match '(1 2)
         (((? fixnum? a) (? fixnum? b)) (list a b)))
      '(1 2))

(test "predicate match fail"
      '(destructuring-match '("a" 2)
         (((? fixnum? a) (? fixnum? b)) 'match)
         (_ 'no-match))
      'no-match)

(test "tail ellipsis match"
      '(destructuring-match '(1 2 3)
         ((a ... b) (list a b)))
      '((1 2) 3))

(test "n-tail ellipsis match"
      '(destructuring-match '(1 2 3 4)
         ((a ... b c) (list a b c)))
      '((1 2) 3 4))

;;;;;

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))

#|
./build/nanos --boot boot/core.ir --script tests/test-module-destruct.scm
|#
