;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

#|
  Destructuring Pattern Matching and Binding

  This module provides two macros for pattern-based decomposition of
  S-expressions:

  (destructuring-match expr clause ...)

    Each clause is (pattern body) or (pattern fender body).
    Returns the body of the first clause whose pattern matches expr
    and whose fender (if present) returns true. Returns #f if no
    clause matches.

  (destructuring-bind pattern expr body ...)

    Binds variables in pattern to the corresponding parts of expr,
    then evaluates body. Signals an error if the pattern does not
    match.

  Pattern language:

    symbol        Binds the matched value to symbol.
    _             Wildcard; matches anything, no binding.
    ()            Matches the empty list.
    (p1 . p2)     Matches a pair; p1 matches car, p2 matches cdr.
    (p ... )      Matches zero or more elements; p is bound to a
                  list of all matched values.
    (p ... . q)   Matches zero or more elements followed by a tail
                  that matches q.
    (p ... q1 q2) Matches zero or more elements followed by fixed
                  trailing elements.
    'datum        Matches datum using eq?, eqv?, or equal? depending
                  on the datum type.
    (? pred)      Matches if (pred value) is true.
    (? pred pat)  Matches if (pred value) is true and pat matches
                  value.

  The compiler optimizes car/cdr chains into caar, cadr, etc. up to
  four levels deep and memoizes repeated subexpressions to avoid
  redundant traversals.
|#

(define-module (core destructuring)
  (export destructuring-match destructuring-bind)
  (import (core let-values))

  (define drop-last-cdr
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? lst) (cons (car lst) (loop (cdr lst))))
                     (else '())))))))

  (define drop-last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
              (let loop ((lst lst))
                (cond ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst))))
                      (else '())))))))

  (define last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
              (let loop ((lst lst))
                (cond ((pair? (cdr lst)) (loop (cdr lst)))
                      (else lst)))))))

  (define last-cdr
    (lambda (lst)
      (cond ((pair? lst)
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (loop (cdr lst)))
                     (else (cdr lst)))))
            (else lst))))

  (define last-n-pair
    (lambda (n lst)
      (let ((lead (let advance ((l lst) (i n))
                    (cond ((= i 0) l)
                          ((pair? l) (advance (cdr l) (- i 1)))
                          (else #f)))))
        (cond (lead
               (let walk ((lead lead) (trail lst))
                 (cond ((pair? lead) (walk (cdr lead) (cdr trail)))
                       (else trail))))
              (else '())))))

  (define drop-last-n-pair
    (lambda (n lst)
      (let ((lead (let advance ((l lst) (i n))
                    (cond ((= i 0) l)
                          ((pair? l) (advance (cdr l) (- i 1)))
                          (else #f)))))
        (cond (lead
               (let walk ((lead lead) (trail lst))
                 (cond ((pair? lead) (cons (car trail) (walk (cdr lead) (cdr trail))))
                       (else '()))))
              (else '())))))

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
                (vector-set! mem 0 (cons (cons e name) (vector-ref mem 0)))
                name)))))

  (define compile-match
    (lambda (ren mem pat ref match bind vars)
      (cond ((quoted-pair? pat)
             (values (cons `(,(choose-pred (cadr pat)) ,ref ',(cadr pat)) match) bind vars))
            ((ellipsis-pair? pat)
             (let ((wild? (eq? (car pat) '_)))
               (cond ((null? (cddr pat))
                      (values (cons `(list? ,ref) match)
                              (if wild? bind (cons ref bind))
                              (if wild? vars (cons (car pat) vars))))
                     ((or (not (pair? (cddr pat))) (predicate-pair? (cddr pat)))
                      (compile-match ren mem (cddr pat) `(last-cdr ,ref) match
                                     (if wild? bind (cons `(drop-last-cdr ,ref) bind))
                                     (if wild? vars (cons (car pat) vars))))
                     ((pair? (cddr pat))
                      (let-values (((tail-ref new-match drop-expr)
                                    (cond ((null? (cdddr pat))
                                           (let ((memoize (memoize-ref `(last-pair ,ref) mem)))
                                             (values memoize
                                                     (cons `(and (pair? ,ref) (set! ,memoize (last-pair ,ref))) match)
                                                     `(drop-last-pair ,ref))))
                                          (else
                                           (let* ((n (- (count-non-dotted-pattern pat) 2))
                                                  (memoize (memoize-ref `(last-n-pair ,n ,ref) mem)))
                                             (values memoize
                                                     (cons `(and (pair? ,ref) (set! ,memoize (last-n-pair ,n ,ref))) match)
                                                     `(drop-last-n-pair ,n ,ref)))))))
                        (compile-match ren mem (cddr pat) tail-ref new-match
                                       (if wild? bind (cons drop-expr bind))
                                       (if wild? vars (cons (car pat) vars)))))
                     (else (values #f #f #f)))))
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
         (let ((datum (gentemp)) (ren (make-eq-hashtable)) (mem (vector '())))
           (let ((code
                   (map (lambda (clause)
                          (syntax-case clause ()
                            ((?pat)
                             (let ((pat (syntax->datum #'?pat)))
                               (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                 (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                       (match (list #f (reverse match) '() '() #'#t #'#t))
                                       (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                            ((?pat ?body)
                             (let ((pat (syntax->datum #'?pat)))
                               (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                 (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                       (match (list #f (reverse match) vars inits #'?body #'#t))
                                       (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                            ((?pat ?fender ?body)
                             (let ((pat (syntax->datum #'?pat)))
                               (let-values (((match inits vars) (compile-match ren mem pat datum '() '() '())))
                                 (cond ((duplicates? vars) (syntax-violation 'destructuring-match "duplicate variables" x pat))
                                       (match (list #f (reverse match) vars inits #'?body #'?fender))
                                       (else (syntax-violation 'destructuring-match "malformed pattern" x pat))))))
                            (_ (syntax-violation 'destructuring-match "malformed clause" x clause))))
                        #'(?clauses ...))))
             (let-values (((shares others) (partition car code)))
               (let ((subexprs (map (lambda (e) `(,(car e) (and ,@(cadr e)))) shares)) (clauses (map cdr others)))
                 (with-syntax
                     ((?datum (datum->syntax #'k datum))
                      (((?pred-lhs ?pred-rhs) ...)
                       (map (lambda (a) (list (datum->syntax #'k (cdr a)) (datum->syntax #'?_ (car a))))
                            (hashtable->alist ren)))
                      ((?mem ...) (map (lambda (e) (datum->syntax #'k (cdr e))) (vector-ref mem 0)))
                      ((?subexprs ...) (map (lambda (e) (datum->syntax #'k e)) subexprs))
                      ((?dispatch ...)
                       (map (lambda (clause)
                              (let-values (((tests vars inits body fender) (apply values clause)))
                                (with-syntax
                                    (((?vars ...) (map (lambda (e) (datum->syntax #'?_ e)) vars))
                                     ((?inits ...) (map (lambda (e) (datum->syntax #'k e)) inits))
                                     ((?tests ...) (map (lambda (e) (datum->syntax #'k e)) tests))
                                     (?fender (datum->syntax #'?_ (syntax->datum fender)))
                                     (?body (datum->syntax #'?_ (syntax->datum body))))
                                  (if (eq? (syntax->datum #'?fender) #t)
                                      #'((and ?tests ...) 
                                         (let ((?vars ?inits) ...) ?body))
                                      #'((and ?tests ... (let ((?vars ?inits) ...) ?fender))
                                         (let ((?vars ?inits) ...) ?body))))))
                            clauses)))
                   #'(let ((?datum ?expr) (?pred-lhs ?pred-rhs) ... (?mem #f) ...)
                       (let* (?subexprs ...) (cond ?dispatch ... (else #f)))))))))))))

  (define-syntax destructuring-bind
    (lambda (x)
      (syntax-case x ()
        ((?_ ?pat ?expr ?body ...)
         (let ((pat (syntax->datum #'?pat))
               (datum (gensym "tmp"))
               (ren (make-eq-hashtable))
               (mem (vector '())))
           (let-values (((match inits vars)
                         (compile-match ren mem pat datum '() '() '())))
             (cond ((not (null? (vector-ref mem 0)))
                    (syntax-violation 'destructuring-bind "tail match not supported" x pat))
                   ((duplicates? vars)
                    (syntax-violation 'destructuring-bind "duplicate variables" x pat))
                   (match
                    (with-syntax
                        ((?datum (datum->syntax #'k datum))
                         ((?vars ...) (map (lambda (e) (datum->syntax #'?_ e)) vars))
                         ((?inits ...) (map (lambda (e) (datum->syntax #'k e)) inits))
                         ((?body ...) (datum->syntax #'?_ (syntax->datum (syntax (?body ...))))))
                      (syntax
                       (let ((?datum ?expr))
                         (let ((?vars ?inits) ...)
                           ?body ...)))))
                   (else
                    (syntax-violation 'destructuring-bind "malformed pattern" x pat)))))))))

  (for-each (lambda (e) (hashtable-set! ca---r (car e) (cdr e)))
            '((car . caar) (cdr . cadr) (caar . caaar) (cadr . caadr) (cdar . cadar) (cddr . caddr)
              (caaar . caaaar) (caadr . caaadr) (cadar . caadar) (caddr . caaddr) (cdaar . cadaar)
              (cdadr . cadadr) (cddar . caddar) (cdddr . cadddr)))

  (for-each (lambda (e) (hashtable-set! cd---r (car e) (cdr e)))
            '((car . cdar) (cdr . cddr) (caar . cdaar) (cadr . cdadr) (cdar . cddar) (cddr . cdddr)
              (caaar . cdaaar) (caadr . cdaadr) (cadar . cadadar) (caddr . cdaddr) (cdaar . cddaar)
              (cdadr . cddadr) (cddar . cdddar) (cdddr . cddddr)))

) ;[end]