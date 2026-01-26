;; syntax_case.scm
;; R6RS-compatible syntax-case implementation for research.
;;
;; Provides syntax-case, syntax, and related procedural macro utilities.

;;=============================================================================
;; SECTION 1: Globals & State
;;=============================================================================

(define *syntax-temp-counter* 0)
(define *current-syntax-bindings* '())
(define *current-syntax-meta-env* '())

;;=============================================================================
;; SECTION 2: Utilities
;;=============================================================================

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (iota n)
  (let loop ((i 0))
    (if (= i n) '() (cons i (loop (+ i 1))))))

;;=============================================================================
;; SECTION 3: Syntax Objects
;;=============================================================================

;; A syntax object is a vector: #(**syntax-object** datum context)
(define (make-syntax-object datum context)
  (if (or (symbol? datum) (pair? datum) (vector? datum))
      (vector '**syntax-object** datum context)
      datum))

(define (syntax-object? obj)
  (and (vector? obj) 
       (= (vector-length obj) 3) 
       (eq? (vector-ref obj 0) '**syntax-object**)))

(define (syntax-object-datum obj)
  (if (syntax-object? obj) (vector-ref obj 1) obj))

(define (syntax-object-context obj)
  (if (syntax-object? obj) (vector-ref obj 2) '()))

(define (syntax->datum obj)
  (cond
    ((syntax-object? obj) (syntax->datum (syntax-object-datum obj)))
    ((pair? obj) (cons (syntax->datum (car obj)) (syntax->datum (cdr obj))))
    ((vector? obj) (list->vector (map syntax->datum (vector->list obj))))
    (else obj)))

(define (datum->syntax template-id datum)
  (make-syntax-object datum (syntax-object-context template-id)))

(define (syntax->list obj)
  (let ((datum (syntax-object-datum obj))
        (ctx (syntax-object-context obj)))
    (cond
      ((null? datum) '())
      ((pair? datum)
       (cons (make-syntax-object (car datum) ctx)
             (syntax->list (make-syntax-object (cdr datum) ctx))))
      (else obj))))

;;=============================================================================
;; SECTION 4: Identifiers
;;=============================================================================

(define (identifier? obj)
  (or (symbol? obj)
      (and (syntax-object? obj) (symbol? (syntax-object-datum obj)))))

(define (bound-identifier=? id1 id2)
  (let ((d1 (syntax-object-datum id1))
        (d2 (syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2)
         (equal? (syntax-object-context id1) (syntax-object-context id2)))))

(define (free-identifier=? id1 id2)
  (eq? (syntax-object-datum id1) (syntax-object-datum id2)))

;;=============================================================================
;; SECTION 5: Pattern Matching
;;=============================================================================

(define (collect-pattern-vars pattern literals ellipsis)
  (cond
    ((and (symbol? pattern) (not (member pattern literals)) 
          (not (eq? pattern '_)) (not (eq? pattern ellipsis)))
     (list pattern))
    ((pair? pattern)
     (append (collect-pattern-vars (car pattern) literals ellipsis)
             (collect-pattern-vars (cdr pattern) literals ellipsis)))
    (else '())))

(define (transpose-matches vars matches)
  (map (lambda (v)
         (cons v (map (lambda (m) (cdr (assq v m))) matches)))
       vars))

(define (syntax-case-match literals pattern input ellipsis)
  (cond
    ;; Literal identifier: must match exactly (free-identifier=?)
    ((and (identifier? pattern) (member (syntax-object-datum pattern) literals))
     (and (identifier? input)
          (free-identifier=? pattern input)
          '()))

    ;; Wildcard
    ((eq? pattern '_) '())

    ;; Pattern variable
    ((symbol? pattern) (list (cons pattern input)))

    ;; Ellipsis pattern (P ... rest)
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (let ((input-list (syntax->list input)))
       (letrec ((match-rest
                 (lambda (xs prefix-matches)
                   (let ((m-rest (syntax-case-match literals (cddr pattern) 
                                                    (if (syntax-object? xs) xs (make-syntax-object xs (syntax-object-context input)))
                                                    ellipsis)))
                     (if m-rest
                         (let* ((vars (if (null? prefix-matches)
                                          (collect-pattern-vars (car pattern) literals ellipsis)
                                          (map car (car prefix-matches))))
                                (p-vars (transpose-matches vars (reverse prefix-matches))))
                           (append p-vars m-rest))
                         #f))))
                (loop (lambda (xs prefix-matches)
                        (let ((m-p (if (pair? xs) (syntax-case-match literals (car pattern) (car xs) ellipsis) #f)))
                          (if m-p
                              (let ((res (loop (cdr xs) (cons m-p prefix-matches))))
                                (if res res (match-rest xs prefix-matches)))
                              (match-rest xs prefix-matches))))))
         (loop input-list '()))))

    ;; Pair
    ((pair? pattern)
     (let ((input-datum (syntax-object-datum input))
           (input-context (syntax-object-context input)))
       (if (pair? input-datum)
           (let ((m1 (syntax-case-match literals (car pattern) (make-syntax-object (car input-datum) input-context) ellipsis))
                 (m2 (syntax-case-match literals (cdr pattern) (make-syntax-object (cdr input-datum) input-context) ellipsis)))
             (and m1 m2 (append m1 m2)))
           #f)))

    ;; Constant
    (else (if (equal? pattern (syntax-object-datum input)) '() #f))))

;;=============================================================================
;; SECTION 6: Template Expansion
;;=============================================================================

(define (syntax-depth-map pattern literals ellipsis depth)
  (cond
    ((and (symbol? pattern) (not (member pattern literals))
          (not (eq? pattern '_)) (not (eq? pattern ellipsis)))
     (list (cons pattern depth)))
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (append (syntax-depth-map (car pattern) literals ellipsis (+ depth 1))
             (syntax-depth-map (cddr pattern) literals ellipsis depth)))
    ((pair? pattern)
     (append (syntax-depth-map (car pattern) literals ellipsis depth)
             (syntax-depth-map (cdr pattern) literals ellipsis depth)))
    (else '())))

(define (expand-syntax template bindings context meta-env depth ellipsis literals)
  (cond
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if (and b (= (or (cdr (assq template meta-env)) 0) depth))
           (cdr b)
           template)))
    ((pair? template)
     (if (and (pair? (cdr template)) (eq? (cadr template) ellipsis))
         ;; Handle ellipsis in template
         (let* ((p (car template))
                (p-vars (collect-pattern-vars p literals ellipsis))
                (drivers (filter (lambda (v)
                                   (let ((pair (assq v meta-env)))
                                     (and pair (> (cdr pair) depth))))
                                 p-vars))
                (len (if (null? drivers)
                         (error "ellipsis in template with no pattern variables" template)
                         (length (cdr (assq (car drivers) bindings)))))
                (new-template-list
                 (map (lambda (i)
                        (let ((iter-bindings
                               (map (lambda (v)
                                      (cons v (list-ref (cdr (assq v bindings)) i)))
                                    drivers)))
                          (expand-syntax p (append iter-bindings bindings) context meta-env (+ depth 1) ellipsis literals)))
                      (iota len))))
           (append new-template-list (expand-syntax (cddr template) bindings context meta-env depth ellipsis literals)))
         (cons (expand-syntax (car template) bindings context meta-env depth ellipsis literals)
               (expand-syntax (cdr template) bindings context meta-env depth ellipsis literals))))
    (else template)))

;;=============================================================================
;; SECTION 7: Quasisyntax & Transformation
;;=============================================================================

(define (extract-quasisyntax x depth literals)
  (cond
    ((pair? x)
     (cond
       ((and (eq? (car x) 'quasisyntax) (pair? (cdr x)) (null? (cddr x)))
        (let ((res (extract-quasisyntax (cadr x) (+ depth 1) literals)))
          (cons `(quasisyntax ,(car res)) (cdr res))))
       
       ((and (eq? (car x) 'unsyntax) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (string->symbol (string-append "unsyntax." (number->string (begin (set! *syntax-temp-counter* (+ *syntax-temp-counter* 1)) *syntax-temp-counter*))))))
              (cons (list '**splice-unsyntax** tmp) (list (list tmp (cadr x)))))
            (let ((res (extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax ,(car res)) (cdr res)))))
       
       ((and (eq? (car x) 'unsyntax-splicing) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (string->symbol (string-append "unsyntax-splicing." (number->string (begin (set! *syntax-temp-counter* (+ *syntax-temp-counter* 1)) *syntax-temp-counter*))))))
              (cons (list '**splice-unsyntax-splicing** tmp) (list (list (list tmp '...) (cadr x)))))
            (let ((res (extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax-splicing ,(car res)) (cdr res)))))
       
       (else
        (let ((car-res (extract-quasisyntax (car x) depth literals))
              (cdr-res (extract-quasisyntax (cdr x) depth literals)))
          (let ((car-tmpl (car car-res))
                (cdr-tmpl (car cdr-res))
                (bindings (append (cdr car-res) (cdr cdr-res))))
            (cond
              ((and (pair? car-tmpl) (eq? (car car-tmpl) '**splice-unsyntax**))
               (cons (cons (cadr car-tmpl) cdr-tmpl) bindings))
              ((and (pair? car-tmpl) (eq? (car car-tmpl) '**splice-unsyntax-splicing**))
               (cons (append (list (cadr car-tmpl) '...) cdr-tmpl) bindings))
              (else
               (cons (cons car-tmpl cdr-tmpl) bindings))))))))
    
    ((vector? x)
     (let ((res (extract-quasisyntax (vector->list x) depth literals)))
       (cons (list->vector (car res)) (cdr res))))
    (else (cons x '()))))

(define (prepare-eval-expr expr literals meta-env bindings)
  (let loop ((x expr))
    (cond
      ((symbol? x)
       (let ((b (assq x bindings)))
         (if (and b (= (or (cdr (assq x meta-env)) 0) 0))
             (let ((val (cdr b)))
               (if (and (pair? val) (eq? (car val) 'quote))
                   val
                   (list 'quote val)))
             x)))
      ((pair? x)
       (cond
         ((eq? (car x) 'syntax)
          `(expand-syntax ',(cadr x) *current-syntax-bindings* '() *current-syntax-meta-env* 0 '... ',literals))
         ((eq? (car x) 'quasisyntax)
          (let ((res (extract-quasisyntax (cadr x) 0 literals)))
            (let ((new-tmpl (car res))
                  (bindings (cdr res)))
              (if (null? bindings)
                  `(expand-syntax ',new-tmpl *current-syntax-bindings* '() *current-syntax-meta-env* 0 '... ',literals)
                  (loop `(with-syntax ,bindings (syntax ,new-tmpl)))))))
         ((eq? (car x) 'syntax-case)
          (let ((input (loop (cadr x)))
                (lits (caddr x))
                (clauses-expr (cons 'list (map (lambda (c)
                                                 (let ((pat (car c))
                                                       (fender (if (null? (cddr c)) #t (cadr c)))
                                                       (output (if (null? (cddr c)) (cadr c) (caddr c))))
                                                   `(list ',pat 
                                                          (lambda () ,(loop fender))
                                                          (lambda () ,(loop output)))))
                                               (cdddr x)))))
            `(expand-syntax-case ,input ',lits ,clauses-expr (interaction-environment))))
          ((eq? (car x) 'with-syntax)
           (let ((b-specs (cadr x))
                 (body (cddr x)))
             `(expand-with-syntax (list 'with-syntax (list ,@(map (lambda (s) `(list ',(car s) ,(loop (cadr s)))) b-specs)) (lambda () ,(loop (cons 'begin body)))) (interaction-environment))))
         ((eq? (car x) 'quote) x)
         (else (cons (loop (car x)) (loop (cdr x))))))
      (else x))))

;;=============================================================================
;; SECTION 8: Macro Expansion Entry Points
;;=============================================================================

(define (generate-temporaries l)
  (let ((lst (syntax->list l)))
    (map (lambda (x)
           (set! *syntax-temp-counter* (+ *syntax-temp-counter* 1))
           (make-syntax-object (string->symbol (string-append "temp." (number->string *syntax-temp-counter*))) '()))
         lst)))

(define (expand-with-syntax expr env)
  (let ((bindings (cadr expr))
        (body-thunk (caddr expr)))
    (expand-syntax-case (make-syntax-object (map cadr bindings) '())
                        '()
                        (list (list (map car bindings) (lambda () #t) body-thunk))
                        env)))

(define (expand-syntax-case input literals clauses env)
  (let ((ellipsis '...))
    (let loop ((clauses clauses))
      (if (null? clauses)
          (error "syntax-case: no matching pattern" input)
          (let* ((clause (car clauses))
                 (pattern (car clause))
                 (fender (cadr clause))
                 (output (caddr clause))
                 (m (syntax-case-match literals pattern input ellipsis)))
            (if m
                (let ((old-bindings *current-syntax-bindings*)
                      (old-meta *current-syntax-meta-env*)
                      (new-meta (syntax-depth-map pattern literals ellipsis 0)))
                  (set! *current-syntax-bindings* (append m old-bindings))
                  (set! *current-syntax-meta-env* (append new-meta old-meta))
                  (let ((matched? (eval-fender fender literals pattern *current-syntax-bindings* env)))
                    (if matched?
                        (let ((res (eval-output output literals pattern *current-syntax-bindings* env)))
                          (set! *current-syntax-bindings* old-bindings)
                          (set! *current-syntax-meta-env* old-meta)
                          res)
                        (begin
                          (set! *current-syntax-bindings* old-bindings)
                          (set! *current-syntax-meta-env* old-meta)
                          (loop (cdr clauses))))))
                (loop (cdr clauses))))))))

(define (eval-fender expr literals pattern bindings env)
  (if (procedure? expr)
      (expr)
      (if (eq? expr #t) #t
          (eval (prepare-eval-expr expr literals *current-syntax-meta-env* bindings) 
                (if (null? env) (interaction-environment) env)))))

(define (eval-output expr literals pattern bindings env)
  (if (procedure? expr)
      (expr)
      (eval (prepare-eval-expr expr literals *current-syntax-meta-env* bindings) 
            (if (null? env) (interaction-environment) env))))
