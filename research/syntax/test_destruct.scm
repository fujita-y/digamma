(load "macroexpand.scm")

(define (test name expr expected)
  (let ((result (eval (macroexpand expr 'strip) (interaction-environment))))
    (if (equal? result expected)
        (display (format "PASS: ~a\n" name))
        (begin
          (display (format "FAIL: ~a\n" name))
          (display (format "  Expected: ~a\n" expected))
          (display (format "  Got:      ~a\n" result))))))


(define generate-temporary-symbol (lambda () (gensym "tmp")))

(define ca---r (make-hash-table))
(define cd---r (make-hash-table))

(for-each (lambda (e) (hash-table-put! ca---r (car e) (cdr e)))
          '((car . caar) (cdr . cadr) (caar . caaar) (cadr . caadr) (cdar . cadar) (cddr . caddr)
            (caaar . caaaar) (caadr . caaadr) (cadar . caadar) (caddr . caaddr) (cdaar . cadaar)
            (cdadr . cadadr) (cddar . caddar) (cdddr . cadddr)))

(for-each (lambda (e) (hash-table-put! cd---r (car e) (cdr e)))
          '((car . cdar) (cdr . cddr) (caar . cdaar) (cadr . cdadr) (cdar . cddar) (cddr . cdddr)
            (caaar . cdaaar) (caadr . cdaadr) (cadar . cadadar) (caddr . cdaddr) (cdaar . cddaar)
            (cdadr . cddadr) (cddar . cdddar) (cdddr . cddddr)))

(define car+
  (lambda (expr)
    (cond ((and (pair? expr) (hash-table-get ca---r (car expr) #f))
           => (lambda (a) (cons a (cdr expr))))
          (else (list 'car expr)))))

(define cdr+
  (lambda (expr)
    (cond ((and (pair? expr) (hash-table-get cd---r (car expr) #f))
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
           (let ((name (generate-temporary-symbol)))
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
                  (let ((memoize (generate-temporary-symbol)))
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
                                                  (cons (car pat) vars)))))))))
                 (else (values #f #f #f))))
          ((predicate-pair? pat)
           (let ((renamed (or (hash-table-get ren (cadr pat) #f) (generate-temporary-symbol))))
             (hash-table-put! ren (cadr pat) renamed)
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

(define reorder-tests
  (lambda (lst preds)
    (let loop ((lst lst) (preds preds) (acc '()))
      (if (null? preds)
          (append acc lst)
          (let-values (((hits rest) (partition (lambda (e) (memq (car e) (car preds))) lst)))
            (loop rest (cdr preds) (append acc hits)))))))

(define reorder
  (lambda (clauses)
    (map (lambda (clause)
           `(,(car clause) ,(reorder-tests (cadr clause) '((pair? and) (null?) (list?))) ,@(cddr clause)))
         clauses)))

(define cse-length
  (lambda (clause1 clause2)
    (let loop ((n 0) (e1 (cadr clause1)) (e2 (cadr clause2)))
      (cond ((or (null? e1) (null? e2) (not (equal? (car e1) (car e2)))) n)
            (else (loop (+ n 1) (cdr e1) (cdr e2)))))))

(define cse-1
  (lambda (clauses current)
    (let* ((cs-lst (map (lambda (clause) (if (eq? clause current) -1 (cse-length clause current))) clauses))
           (cs-max (let ((lst (filter (lambda (n) (>= n 2)) cs-lst)))
                     (if (null? lst) 0 (apply min lst)))))
      (and (>= cs-max 2)
           (let* ((cs-tag (generate-temporary-symbol))
                  (clause1 (list-head (cadr current) cs-max))
                  (clause2 (map (lambda (clause len)
                                  (if (or (= len -1) (>= len cs-max))
                                      `(,(car clause) ,(cons cs-tag (list-tail (cadr clause) cs-max)) ,@(cddr clause))
                                      clause))
                                clauses cs-lst)))
             (if (symbol? (car clause1))
                 (append clause2 (list (list cs-tag clause1)))
                 (cons (list cs-tag clause1) clause2)))))))

(define cse
  (lambda (clauses)
    (let loop ((lst clauses))
      (if (null? lst)
          clauses
          (cond ((cse-1 clauses (car lst)) => cse)
                (else (loop (cdr lst))))))))

(macroexpand
 '(define-syntax destructuring-match
    (lambda (x)
      (syntax-case x ()
        ((?_ ?expr ?clauses ...)
         (let ((datum (generate-temporary-symbol))
               (ren (make-hash-table))
               (mem (vector '())))
           (let ((code (cse (reorder (map (lambda (clause)
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
                                         (syntax (?clauses ...)))))))
             (let-values (((shares others) (partition car code)))
               (let ((subexprs (map (lambda (e) `(,(car e) (and ,@(cadr e)))) shares))
                     (clauses (map cdr others)))
                 (with-syntax ((?datum (datum->syntax (syntax k) datum))
                               (((?pred-lhs ?pred-rhs) ...)
                                (map (lambda (a)
                                       (list (datum->syntax (syntax k) (cdr a))
                                             (datum->syntax (syntax ?_) (car a))))
                                     (hash-table->alist ren)))
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
                                               (syntax ((and ?tests ...
                                                             (let ((?vars ?inits) ...) ?fender))
                                                        (let ((?vars ?inits) ...) ?body)))))))
                                     clauses)))
                   (syntax (let ((?datum ?expr) (?pred-lhs ?pred-rhs) ... (?mem #f) ...)
                             (let* (?subexprs ...)
                               (cond ?dispatch ... (else #f)))))))))))))))

(test "simple match"
  '(destructuring-match '(quote 1)
     (('quote e) (list 1))
     (_ (list 'nomatch)))
  '(1))
