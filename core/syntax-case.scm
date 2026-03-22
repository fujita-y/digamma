;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R6RS-compatible syntax-case implementation
;;
;; This module provides the procedural macro system based on syntax-case:
;; - syntax-case: pattern matching with fenders for procedural macros
;; - syntax: template expansion with ellipsis handling
;; - quasisyntax/unsyntax: quasi-quoting for syntax templates
;; - with-syntax: local pattern binding within transformers
;; - generate-temporaries: create fresh identifiers
;; - Variable transformers: intercept set! forms
;;
;; Key concepts:
;; - Syntax objects wrap datums with lexical context information
;; - Pattern matching binds pattern variables to matched subterms
;; - Thunks defer evaluation of fenders and outputs for proper scoping
;; - Meta-environment tracks ellipsis depth of pattern variables

;;=============================================================================
;; 1. Globals & State
;;=============================================================================

;; Current pattern variable bindings: ((var . value) ...)
(define *current-syntax-bindings* '())

;; Current meta-environment tracking ellipsis depth: ((var . depth) ...)
(define *current-syntax-meta-env* '())

;;=============================================================================
;; 2. Utilities & Error Handling
;;=============================================================================

;; Signal a syntax violation error.
(define (syntax-violation who message form . subform)
  (let ((who-str (if who (format "~a: " who) "")))
    (if (null? subform)
        (error (format "~asyntax-violation: ~a in ~s" who-str message form))
        (error (format "~asyntax-violation: ~a in ~s (subform: ~s)" who-str message form (car subform))))))

;;=============================================================================
;; 3. Syntax Objects & Identifiers
;;=============================================================================

;; Syntax objects wrap datums with lexical context information.
;; A syntax object is a 3-element vector: #(**syntax-object** datum context)
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

;; Convert a syntax object to a plain datum by stripping all context.
(define (syntax->datum obj)
  (cond
    ((syntax-object? obj) (syntax->datum (syntax-object-datum obj)))
    ((pair? obj) (cons (syntax->datum (car obj)) (syntax->datum (cdr obj))))
    ((vector? obj) (list->vector (map syntax->datum (vector->list obj))))
    (else obj)))

;; Inheritance mechanism for context during transformation.
(define (datum->syntax template-id datum)
  (make-syntax-object datum (syntax-object-context template-id)))

;; Variable transformers for set! interception.
(define (make-variable-transformer proc)
  (vector '**variable-transformer** proc))

(define (variable-transformer? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) '**variable-transformer**)))

(define (variable-transformer-procedure obj)
  (vector-ref obj 1))

;; Convert syntax object containing a list into a list of syntax objects.
(define (syntax->list obj)
  (let ((datum (syntax-object-datum obj))
        (ctx (syntax-object-context obj)))
    (cond
      ((null? datum) '())
      ((pair? datum)
       (cons (make-syntax-object (car datum) ctx)
             (syntax->list (make-syntax-object (cdr datum) ctx))))
      (else obj))))

(define (identifier? obj)
  (or (symbol? obj)
      (and (syntax-object? obj) (symbol? (syntax-object-datum obj)))))

;; Hygiene-aware identifier equality.
(define (bound-identifier=? id1 id2)
  (let* ((d1 (syntax-object-datum id1))
         (d2 (syntax-object-datum id2))
         (ctx1 (if (symbol? id1)
                   (let ((entry (assq id1 *rename-env*)))
                     (if entry (caddr entry) '()))
                   (syntax-object-context id1)))
         (ctx2 (if (symbol? id2)
                   (let ((entry (assq id2 *rename-env*)))
                     (if entry (caddr entry) '()))
                   (syntax-object-context id2))))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2)
         (equal? ctx1 ctx2))))

;; Name-only identifier equality (for literals).
(define (free-identifier=? id1 id2)
  (let ((d1 (syntax-object-datum id1))
        (d2 (syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2))))

;;=============================================================================
;; 4. Pattern Matching Logic
;;=============================================================================

;; Collect all pattern variable names from a pattern.
(define (collect-pattern-vars pattern literals ellipsis)
  (cond
    ((and (symbol? pattern)
          (not (member pattern literals))
          (not (eq? pattern '_))
          (not (eq? pattern ellipsis)))
     (list pattern))
    ((pair? pattern)
     (append (collect-pattern-vars (car pattern) literals ellipsis)
             (collect-pattern-vars (cdr pattern) literals ellipsis)))
    (else '())))

;; Group values matched by an ellipsis pattern by variable.
(define (transpose-matches vars matches)
  (map (lambda (v)
         (cons v (map (lambda (m) (cdr (assq v m))) matches)))
       vars))

;; Match input syntax against a pattern.
(define (syntax-case-match literals pattern input ellipsis)
  (cond
    ((and (identifier? pattern) (member (syntax-object-datum pattern) literals))
     (and (identifier? input)
          (free-identifier=? pattern input)
          '()))
    ((eq? pattern '_) '())
    ((symbol? pattern) (list (cons pattern input)))
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (let* ((input-list (syntax->list input))
            (p-vars (collect-pattern-vars (car pattern) literals ellipsis)))
       (letrec ((match-rest
                 (lambda (xs prefix-matches)
                   (let ((m-rest (syntax-case-match literals (cddr pattern)
                                                      (if (syntax-object? xs) xs (make-syntax-object xs (syntax-object-context input)))
                                                      ellipsis)))
                     (if m-rest
                         (let ((transposed (transpose-matches p-vars (reverse prefix-matches))))
                           (append transposed m-rest))
                         #f))))
                (loop (lambda (xs prefix-matches)
                        (let ((m-p (if (pair? xs) (syntax-case-match literals (car pattern) (car xs) ellipsis) #f)))
                          (if m-p
                              (let ((res (loop (cdr xs) (cons m-p prefix-matches))))
                                (if res res (match-rest xs prefix-matches)))
                              (match-rest xs prefix-matches))))))
         (loop input-list '()))))
    ((pair? pattern)
     (let ((input-datum (syntax-object-datum input))
           (input-context (syntax-object-context input)))
       (if (pair? input-datum)
           (let ((m1 (syntax-case-match literals (car pattern) (make-syntax-object (car input-datum) input-context) ellipsis))
                 (m2 (syntax-case-match literals (cdr pattern) (make-syntax-object (cdr input-datum) input-context) ellipsis)))
             (and m1 m2 (append m1 m2)))
           #f)))
    (else (if (equal? pattern (syntax-object-datum input)) '() #f))))

;;=============================================================================
;; 5. Template Expansion
;;=============================================================================

;; Map pattern variables to their ellipsis depths for expansion.
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

;; Substitute bound pattern variables and replicate ellipsis patterns.
(define (expand-syntax template bindings context meta-env depth ellipsis literals suffix)
  (cond
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if (and b (= (or (cdr (assq template meta-env)) 0) depth))
           (cdr b)
           (if (memq template literals)
               template
               (let ((new-sym (rename-symbol template suffix)))
                 (register-renamed! new-sym template context)
                 new-sym)))))
    ((pair? template)
     (if (and (pair? (cdr template)) (eq? (cadr template) ellipsis))
         (let* ((p (car template))
                (p-vars (collect-pattern-vars p literals ellipsis))
                (drivers (filter (lambda (v)
                                   (let ((pair (assq v meta-env)))
                                     (and pair (> (cdr pair) depth))))
                                 p-vars))
                (len (if (null? drivers)
                         (syntax-violation 'syntax "ellipsis in template with no pattern variables" template)
                         (length (cdr (assq (car drivers) bindings)))))
                (new-template-list
                 (map (lambda (i)
                        (let ((iter-bindings
                               (map (lambda (v)
                                      (cons v (list-ref (cdr (assq v bindings)) i)))
                                    drivers)))
                          (expand-syntax p (append iter-bindings bindings) context meta-env (+ depth 1) ellipsis literals suffix)))
                      (iota len))))
           (append new-template-list (expand-syntax (cddr template) bindings context meta-env depth ellipsis literals suffix)))
         (cons (expand-syntax (car template) bindings context meta-env depth ellipsis literals suffix)
               (expand-syntax (cdr template) bindings context meta-env depth ellipsis literals suffix))))
    (else template)))

;;=============================================================================
;; 6. Quasisyntax & Transformation Helpers
;;=============================================================================

;; Handle unsyntax and unsyntax-splicing within quasisyntax.
(define (extract-quasisyntax x depth literals)
  (cond
    ((pair? x)
     (cond
       ((and (eq? (car x) 'quasisyntax) (pair? (cdr x)) (null? (cddr x)))
        (let ((res (extract-quasisyntax (cadr x) (+ depth 1) literals)))
          (cons `(quasisyntax ,(car res)) (cdr res))))
       ((and (eq? (car x) 'unsyntax) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (generate-temporary-symbol "unsyntax-")))
              (cons (list '**splice-unsyntax** tmp) (list (list tmp (cadr x)))))
            (let ((res (extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax ,(car res)) (cdr res)))))
       ((and (eq? (car x) 'unsyntax-splicing) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (generate-temporary-symbol "unsyntax-splicing-")))
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

;; Compile fender and output expressions into runtime calls.
(define (prepare-eval-expr expr literals meta-env bindings context)
  (let ((r-env (if (and (pair? context) (pair? (cddr context))) (caddr context) '())))
    (let loop ((x expr))
      (cond
        ((symbol? x)
         (let ((b (assq x bindings)))
           (if (and b (= (or (cdr (assq x meta-env)) 0) 0))
               (let ((val (cdr b)))
                 (if (and (pair? val) (eq? (car val) 'quote)) val (list 'quote val)))
               (let ((rb (assq x r-env)))
                 (if rb (cdr rb) x)))))
        ((pair? x)
         (cond
           ((eq? (car x) 'syntax)
            `(let ((suffix (fresh-suffix)))
               (expand-syntax ',(cadr x) *current-syntax-bindings* ',context *current-syntax-meta-env* 0 '... ',literals suffix)))
           ((eq? (car x) 'quasisyntax)
            (let ((res (extract-quasisyntax (cadr x) 0 literals)))
              (let ((new-tmpl (car res)) (bindings (cdr res)))
                (if (null? bindings)
                    `(let ((suffix (fresh-suffix)))
                       (expand-syntax ',new-tmpl *current-syntax-bindings* ',context *current-syntax-meta-env* 0 '... ',literals suffix))
                    (loop `(with-syntax ,bindings (syntax ,new-tmpl)))))))
           ((eq? (car x) 'syntax-case)
            (let ((input (loop (cadr x))) (lits (caddr x))
                  (clauses-expr (cons 'list (map (lambda (c)
                                                   (let ((pat (car c))
                                                         (fender (if (null? (cddr c)) #t (cadr c)))
                                                         (output (if (null? (cddr c)) (cadr c) (caddr c)))
                                                         (new-lits (append (caddr x) literals)))
                                                     `(list ',pat
                                                             (lambda () ,(prepare-eval-expr fender new-lits meta-env bindings context))
                                                             (lambda () ,(prepare-eval-expr output new-lits meta-env bindings context)))))
                                                 (cdddr x)))))
              `(expand-syntax-case ,input ',lits ,clauses-expr (current-environment))))
           ((eq? (car x) 'with-syntax)
            (let ((b-specs (cadr x)) (body (cddr x)))
              `(expand-with-syntax (list 'with-syntax (list ,@(map (lambda (s) `(list ',(car s) ,(loop (cadr s)))) b-specs)) (lambda () ,(loop (cons 'begin body)))) (current-environment))))
           ((eq? (car x) 'quote) x)
           (else
            (let ((head (car x)))
              (if (symbol? head)
                  (let* ((m-env (if (pair? context) (car context) '()))
                         (s-env (if (and (pair? context) (pair? (cdr context))) (cadr context) '()))
                         (transformer (and (not (memq head s-env)) (lookup-macro head m-env))))
                    (if transformer
                        (loop (unrename-core (call-transformer transformer x m-env s-env r-env) context))
                        (cons (loop (car x)) (loop (cdr x)))))
                  (cons (loop (car x)) (loop (cdr x))))))))
        (else x)))))

;;=============================================================================
;; 7. Runtime Expansion API
;;=============================================================================

;; Evaluate a transformer expression (fender or output).
(define (eval-transformer-expr expr literals pattern bindings env)
  (if (procedure? expr)
      (expr)
      (if (eq? expr #t)
          #t
          (core-eval (
              prepare-eval-expr expr literals *current-syntax-meta-env* bindings '())
              (if (null? env) (current-environment) env)))))

;; Main syntax-case expansion engine.
(define (expand-syntax-case input literals clauses env)
  (let ((ellipsis '...))
    (let loop ((clauses clauses))
      (if (null? clauses)
          (syntax-violation 'syntax-case "no matching pattern" input)
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
                  (let ((matched? (eval-transformer-expr fender literals pattern *current-syntax-bindings* env)))
                    (if matched?
                        (let ((res (eval-transformer-expr output literals pattern *current-syntax-bindings* env)))
                          (set! *current-syntax-bindings* old-bindings)
                          (set! *current-syntax-meta-env* old-meta)
                          res)
                        (begin
                          (set! *current-syntax-bindings* old-bindings)
                          (set! *current-syntax-meta-env* old-meta)
                          (loop (cdr clauses))))))
                (loop (cdr clauses))))))))

;; Local pattern binding (with-syntax).
(define (expand-with-syntax expr env)
  (let ((bindings (cadr expr))
        (body-thunk (caddr expr)))
    (expand-syntax-case (make-syntax-object (map cadr bindings) '())
                        '()
                        (list (list (map car bindings) (lambda () #t) body-thunk))
                        env)))

;; Fresh identifier generator.
(define (generate-temporaries l)
  (let ((lst (syntax->list l)))
    (map (lambda (x) (make-syntax-object (generate-temporary-symbol "temp.") '())) lst)))
