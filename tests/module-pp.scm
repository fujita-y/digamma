(define-module (core let-values)
  (export let-values let*-values)
  (begin
  (define-syntax let-values
    (syntax-rules ()
      ((let-values () body1 body2 ...)
       (let () body1 body2 ...))
      ((let-values (((v ...) expr) rest ...) body1 body2 ...)
       (call-with-values
       (lambda () expr)
       (lambda (v ...)
         (let-values (rest ...) body1 body2 ...))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () body1 body2 ...)
       (let () body1 body2 ...))
      ((let*-values (binding rest ...) body1 body2 ...)
       (let-values (binding)
         (let*-values (rest ...) body1 body2 ...)))))))

(define-module (core parameterize) 
(export parameterize)
(begin

(define-syntax parameterize-aux
  (syntax-rules ()
    ((_ () ((save new param value) ...) body ...)
     (let ((save #f) ... (new value) ...)
        (dynamic-wind
        (lambda () (set! save (param)) ... (param new) ...)
        (lambda () body ...)
        (lambda () (param save) ...))))
    ((_ ((e1 e2) . more) (stash ...) body ...)
     (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((e1 e2) ...) body ...)
     (parameterize-aux ((e1 e2) ...) () body ...))))

))

(define-module (core destructuring-match) 
(export destructuring-match)
(import (core let-values))
(begin

(define generate-temporary-symbol (lambda () (gensym "tmp")))

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
           (let ((renamed (or (hashtable-ref ren (cadr pat) #f) (generate-temporary-symbol))))
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

(define-syntax destructuring-match
    (lambda (x)
      (syntax-case x ()
        ((?_ ?expr ?clauses ...)
         (let ((datum (generate-temporary-symbol))
               (ren (make-eq-hashtable))
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
                                               (syntax ((and ?tests ...
                                                             (let ((?vars ?inits) ...) ?fender))
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

;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-module (core pretty-print) 
(export pretty-print)
(import (core destructuring-match) (core parameterize))
(begin

#|

  Scheme implementation of "Strictly Pretty, Christian Lindig, March 2000 (http://www.st.cs.uni-sb.de/~lindig)"

  * OCaml code from paper

      let (^^) x y    = DocCons(x,y)
      let empty       = DocNil
      let text s      = DocText(s)
      let nest i x    = DocNest(i,x)
      let break       = DocBreak(" ")
      let breakWith s = DocBreak(s)
      let group d     = DocGroup(d)

      let rec sdocToString = function
          | SNil -> ""
          | SText(s,d) -> s ^ sdocToString d
          | SLine(i,d) -> let prefix = String.make i ’ ’
                          in nl ^ prefix ^ sdocToString d

      let rec fits w = function
          | _ when w < 0               -> false
          | []                         -> true
          | (i,m,DocNil)          :: z -> fits w z
          | (i,m,DocCons(x,y))    :: z -> fits w ((i,m,x)::(i,m,y)::z)
          | (i,m,DocNest(j,x))    :: z -> fits w ((i+j,m,x)::z)
          | (i,m,DocText(s))      :: z -> fits (w - strlen s) z
          | (i,Flat, DocBreak(s)) :: z -> fits (w - strlen s) z
          | (i,Break,DocBreak(_)) :: z -> true (* impossible *)
          | (i,m,DocGroup(x))     :: z -> fits w ((i,Flat,x)::z)

      let rec format w k = function
          | [] -> SNil
          | (i,m,DocNil)          :: z -> format w k z
          | (i,m,DocCons(x,y))    :: z -> format w k ((i,m,x)::(i,m,y)::z)
          | (i,m,DocNest(j,x))    :: z -> format w k ((i+j,m,x)::z)
          | (i,m,DocText(s))      :: z -> SText(s,format w (k + strlen s) z)
          | (i,Flat, DocBreak(s)) :: z -> SText(s,format w (k + strlen s) z)
          | (i,Break,DocBreak(s)) :: z -> SLine(i,format w i z)
          | (i,m,DocGroup(x))     :: z -> if fits (w-k) ((i,Flat,x)::z)
                                          then format w k ((i,Flat ,x)::z)
                                          else format w k ((i,Break,x)::z)
  scheme implementation note:

    Flat           -->  .&FLAT
    Break          -->  .&BREAK
    DocCons(x,y)   -->  (x . y)
    DocNil         -->  ()
    DocText(s)     -->  "s"
    DocNest(i,x)   -->  (.&NEST i x ...)
    DocBreak(" ")  -->  #\;
    DocBreak(s)    -->  #\;
    DocGroup(x)    -->  (.&GROUP x ...)

|#

(define pretty-print-line-length (make-parameter 100))
(define pretty-print-initial-indent (make-parameter 0))
(define pretty-print-maximum-lines (make-parameter #f))
(define pretty-print-unwrap-syntax (make-parameter #f))

(define pretty-print
  (lambda (expr . port)
    (let ((port (if (pair? port) (car port) (current-output-port)))
          (n-more-lines (and (pretty-print-maximum-lines) (- (pretty-print-maximum-lines) 1))))
      (define indent-type1?
        (lambda (id)
          (memq id '(library define-library define define-syntax define-macro define-inline define-constant
                     syntax-rules lambda let letrec let* letrec letrec* let-values let*-values
                     destructuring-match parameterize))))
      (define indent-type2?
        (lambda (id)
          (memq id '(if cond case and or set! import export cons map))))
      (define indent-type3?
        (lambda (id)
          (memq id '(do let-optionals))))
      (define indent-type4?
        (lambda (id)
          (memq id '(let-syntax letrec-syntax with-syntax))))
      (define fits?
        (lambda (w lst)
          (and (>= w 0)
               (or (null? lst)
                   (destructuring-match lst
                     (((_ _ ()) . z) (fits? w z))
                     (((_ '.&BREAK #\;) . z) #t)
                     (((_ '.&FLAT #\;) . z) (fits? (- w 1) z))
                     (((_ _ (? string? s)) . z) (fits? (- w (string-length s)) z))
                     (((i _ ('.&GROUP . x)) . z) (fits? w `((,i .&FLAT ,x) ,@z)))
                     (((i m ('.&NEST j . x)) . z) (fits? w `((,(+ i j) ,m ,x) ,@z)))
                     (((i m (x . y)) . z) (fits? w `((,i ,m ,x) (,i ,m ,y) ,@z))))))))
      (define print
        (lambda (w k lst)
          (or (null? lst)
              (destructuring-match lst
                (((_ _ ()) . z) (print w k z))
                (((i '.&BREAK #\;) . z)
                 (cond ((or (eq? n-more-lines #f) (> n-more-lines 0))
                        (and n-more-lines (set! n-more-lines (- n-more-lines 1)))
                        (put-char port #\newline)
                        (let loop ((i i)) (and (> i 0) (put-char port #\space) (loop (- i 1))))
                        (print w i z))))
                (((_ '.&FLAT #\;) . z)
                 (begin
                   (put-char port #\space)
                   (print w (+ k 1) z)))
                (((_ _ (? string? s)) . z)
                 (begin
                   (put-string port s)
                   (print w (+ k (string-length s)) z)))
                (((i _ ('.&GROUP . x)) . z)
                 (let ((flat `((,i .&FLAT ,x) ,@z)))
                   (if (fits? (- w k) flat)
                       (print w k flat)
                       (print w k `((,i .&BREAK ,x) ,@z)))))
                (((i m ('.&NEST j . x)) . z)
                 (print w k `((,(+ i j) ,m ,x) ,@z)))
                (((i m (x . y)) . z)
                 (print w k `((,i ,m ,x) (,i ,m ,y) ,@z)))))))
      (define symbol->length
        (lambda (obj)
          (string-length (symbol->string obj))))
      (define parse-list
        (lambda (lst)
          (cond ((null? lst) '())
                ((null? (cdr lst))
                 (list (parse (car lst))))
                ((and (eq? (car lst) 'unquote) (pair? (cdr lst)) (null? (cddr lst)))
                 (list "." #\; "," (parse (cadr lst))))
                ((pair? (cdr lst))
                 (cons* (parse (car lst)) #\; (parse-list (cdr lst))))
                (else
                 (list (parse (car lst)) #\; "." #\; (parse (cdr lst)))))))
      (define parse
        (lambda (obj)
          (cond ((pair? obj)
                 (destructuring-match obj
                   (('quote e) `("'" (.&NEST 1 ,(parse e))))
                   (('unquote e) `("," (.&NEST 1 ,(parse e))))
                   (('quasiquote e) `("`" (.&NEST 1 ,(parse e))))
                   (('unquote-splicing e) `(",@" (.&NEST 2 ,(parse e))))
                   (('syntax e) `("#'" (.&NEST 2 ,(parse e))))
                   (('quasisyntax e) `("#`" (.&NEST 2 ,(parse e))))
                   (('unsyntax e) `("#," (.&NEST 2 ,(parse e))))
                   (('unsyntax-splicing e) `("#,@" (.&NEST 3 ,(parse e))))
                   ;; named let
                   (('let (? symbol? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(let ~a " e1)
                              (.&NEST 2
                                      (.&NEST ,(+ (symbol->length e1) 4)
                                              ,(parse e2))
                                      #\;
                                      ,@(parse-list e3) ")")))
                   ;; syntax-rules with <ellipsis>
                   (('syntax-rules (? symbol? e1) (? list? e2) . (? pair? e3))
                    `(.&GROUP (.&NEST 2 (.&GROUP "(syntax-rules" #\; ,(parse e1) #\; ,(parse e2))
                                        #\;
                                        ,@(parse-list e3)) ")"))
                   ;; syntax-case
                   (('syntax-case e1 (? list? e2) . (? pair? e3))
                    `(.&GROUP (.&NEST 2 (.&GROUP "(syntax-case" #\; ,(parse e1) #\; ,(parse e2))
                                        #\;
                                        ,@(parse-list e3)) ")"))
                   (((? indent-type1? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST 2
                                      (.&NEST ,(symbol->length e1)
                                              ,(parse e2))
                                      #\;
                                      ,@(parse-list e3) ")")))
                   (((? indent-type2? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST ,(+ (symbol->length e1) 2)
                                      ,(parse e2)
                                      #\;
                                      ,@(parse-list e3)) ")"))
                   (((? indent-type3? e1) e2 e3 . (? pair? e4))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST 2
                                      (.&NEST 2
                                              ,(parse e2) #\; ,(parse e3))
                                      #\;
                                      ,@(parse-list e4) ")")))
                   (((? indent-type4? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(~a" e1)
                              (.&NEST 2
                                      (.&NEST 2
                                              #\;
                                              ,(parse e2))
                                      #\;
                                      ,@(parse-list e3)) ")"))
                   (('else . _)
                    `(.&GROUP "(" (.&NEST 1 ,@(parse-list obj)) ")"))
                   (('_ . _)
                    `(.&GROUP "(" (.&NEST 1 ,@(parse-list obj)) ")"))
                   (((? symbol? _) . _)
                    `(.&GROUP "(" (.&NEST 2 ,@(parse-list obj)) ")"))
                   (_
                    `(.&GROUP "(" (.&NEST 1 ,@(parse-list obj)) ")"))))
                ((vector? obj)
                 (if (= (vector-length obj) 0)
                     "#()"
                     `(.&GROUP "#(" (.&NEST 2 ,@(parse-list (vector->list obj))) ")")))
                ((tuple? obj)
                 (format "~w" obj))
                ((pretty-print-unwrap-syntax)
                 (format "~u" obj))
                (else
                 (format "~s" obj)))))
      (if (cyclic-object? expr)
          (format port "~w" expr)
          (let ((width (pretty-print-line-length)))
            (parameterize ((collect-notify #f))
              (print width 0 `((,(pretty-print-initial-indent) .&FLAT ,(parse expr)))))))
      (cond ((and n-more-lines (<= n-more-lines 0))
             (put-char port #\newline)
             (let loop ((i (pretty-print-initial-indent))) (and (> i 0) (put-char port #\space) (loop (- i 1))))
             (put-string port "  ..."))
            (else
             (unspecified))))))

))                               

#|
./build/nanos --boot boot/core.ir --script tests/test-module-pp.scm

run 'gosh boot/build-core-ir.scm' to update boot/core.ir, then run ./nanos --boot boot/core.ir --script tests/module.scm to check fix

|#

(import-module (core destructuring-match))
(import-module (core pretty-print))
