;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R6RS-compatible syntax-case implementation
;;
;; Description: Provides syntax-case, syntax, and related procedural macro utilities.
;; This implementation focus on lexical scoping within transformers by using thunks
;; for delayed evaluation of fenders and outputs.

(load "./syntax_common.scm")

;;=============================================================================
;; 1. Globals & State
;;=============================================================================

(define sc:*syntax-temp-counter* 0)
(define sc:*current-syntax-bindings* '())
(define sc:*current-syntax-meta-env* '())

;;=============================================================================
;; 2. Utilities
;;=============================================================================



(define (syntax-violation who message form . subform)
  (let ((who-str (if who (format "~a: " who) "")))
    (if (null? subform)
        (error (format "~asyntax-violation: ~a in ~s" who-str message form))
        (error (format "~asyntax-violation: ~a in ~s (subform: ~s)" who-str message form (car subform))))))

;;=============================================================================
;; 3. Syntax Objects & Identifiers
;;=============================================================================

;; A syntax object is a vector: #(**syntax-object** datum context)
(define (sc:make-syntax-object datum context)
  (if (or (symbol? datum) (pair? datum) (vector? datum))
      (vector '**syntax-object** datum context)
      datum))

(define (sc:syntax-object? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) '**syntax-object**)))

(define (sc:syntax-object-datum obj)
  (if (sc:syntax-object? obj) (vector-ref obj 1) obj))

(define (sc:syntax-object-context obj)
  (if (sc:syntax-object? obj) (vector-ref obj 2) '()))

(define (syntax->datum obj)
  (cond
    ((sc:syntax-object? obj) (syntax->datum (sc:syntax-object-datum obj)))
    ((pair? obj) (cons (syntax->datum (car obj)) (syntax->datum (cdr obj))))
    ((vector? obj) (list->vector (map syntax->datum (vector->list obj))))
    (else obj)))

(define (datum->syntax template-id datum)
  (sc:make-syntax-object datum (sc:syntax-object-context template-id)))

(define (make-variable-transformer proc)
  (vector '**variable-transformer** proc))

(define (sc:variable-transformer? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) '**variable-transformer**)))

(define (sc:variable-transformer-procedure obj)
  (vector-ref obj 1))

(define (sc:syntax->list obj)
  (let ((datum (sc:syntax-object-datum obj))
        (ctx (sc:syntax-object-context obj)))
    (cond
      ((null? datum) '())
      ((pair? datum)
       (cons (sc:make-syntax-object (car datum) ctx)
             (sc:syntax->list (sc:make-syntax-object (cdr datum) ctx))))
      (else obj))))

(define (sc:identifier? obj)
  (or (symbol? obj)
      (and (sc:syntax-object? obj) (symbol? (sc:syntax-object-datum obj)))))

(define (sc:bound-identifier=? id1 id2)
  (let ((d1 (sc:syntax-object-datum id1))
        (d2 (sc:syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2)
         (equal? (sc:syntax-object-context id1) (sc:syntax-object-context id2)))))

(define (sc:free-identifier=? id1 id2)
  (let ((d1 (sc:syntax-object-datum id1))
        (d2 (sc:syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2))))

;;=============================================================================
;; 4. Pattern Matching
;;=============================================================================

(define (sc:collect-pattern-vars pattern literals ellipsis)
  (cond
    ((and (symbol? pattern)
          (not (member pattern literals))
          (not (eq? pattern '_))
          (not (eq? pattern ellipsis)))
     (list pattern))
    ((pair? pattern)
     (append (sc:collect-pattern-vars (car pattern) literals ellipsis)
             (sc:collect-pattern-vars (cdr pattern) literals ellipsis)))
    (else '())))

(define (sc:transpose-matches vars matches)
  (map (lambda (v)
         (cons v (map (lambda (m) (cdr (assq v m))) matches)))
       vars))

(define (sc:syntax-case-match literals pattern input ellipsis)
  (cond
    ;; Literal identifier: must match exactly (sc:free-identifier=?)
    ((and (sc:identifier? pattern) (member (sc:syntax-object-datum pattern) literals))
     (and (sc:identifier? input)
          (sc:free-identifier=? pattern input)
          '()))

    ;; Wildcard
    ((eq? pattern '_) '())

    ;; Pattern variable
    ((symbol? pattern) (list (cons pattern input)))

    ;; Ellipsis pattern (P ... rest)
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (let* ((input-list (sc:syntax->list input))
            (p-vars (sc:collect-pattern-vars (car pattern) literals ellipsis)))
       (letrec ((match-rest
                 (lambda (xs prefix-matches)
                   (let ((m-rest (sc:syntax-case-match literals (cddr pattern)
                                                     (if (sc:syntax-object? xs) xs (sc:make-syntax-object xs (sc:syntax-object-context input)))
                                                     ellipsis)))
                     (if m-rest
                         (let ((transposed (sc:transpose-matches p-vars (reverse prefix-matches))))
                           (append transposed m-rest))
                         #f))))
                (loop (lambda (xs prefix-matches)
                        (let ((m-p (if (pair? xs) (sc:syntax-case-match literals (car pattern) (car xs) ellipsis) #f)))
                          (if m-p
                              (let ((res (loop (cdr xs) (cons m-p prefix-matches))))
                                (if res res (match-rest xs prefix-matches)))
                              (match-rest xs prefix-matches))))))
         (loop input-list '()))))

    ;; Pair
    ((pair? pattern)
     (let ((input-datum (sc:syntax-object-datum input))
           (input-context (sc:syntax-object-context input)))
       (if (pair? input-datum)
           (let ((m1 (sc:syntax-case-match literals (car pattern) (sc:make-syntax-object (car input-datum) input-context) ellipsis))
                 (m2 (sc:syntax-case-match literals (cdr pattern) (sc:make-syntax-object (cdr input-datum) input-context) ellipsis)))
             (and m1 m2 (append m1 m2)))
           #f)))

    ;; Constant
    (else (if (equal? pattern (sc:syntax-object-datum input)) '() #f))))

;;=============================================================================
;; 5. Template Expansion
;;=============================================================================

(define (sc:syntax-depth-map pattern literals ellipsis depth)
  (cond
    ((and (symbol? pattern) (not (member pattern literals))
          (not (eq? pattern '_)) (not (eq? pattern ellipsis)))
     (list (cons pattern depth)))
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (append (sc:syntax-depth-map (car pattern) literals ellipsis (+ depth 1))
             (sc:syntax-depth-map (cddr pattern) literals ellipsis depth)))
    ((pair? pattern)
     (append (sc:syntax-depth-map (car pattern) literals ellipsis depth)
             (sc:syntax-depth-map (cdr pattern) literals ellipsis depth)))
    (else '())))

(define (sc:expand-syntax template bindings context meta-env depth ellipsis literals suffix)
  (cond
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if (and b (= (or (cdr (assq template meta-env)) 0) depth))
           (cdr b)
           (if (memq template literals)
               template
               (let ((new-sym (mc:rename-symbol template suffix)))
                 (mc:register-renamed! new-sym template context)
                 new-sym)))))
    ((pair? template)
     (if (and (pair? (cdr template)) (eq? (cadr template) ellipsis))
         ;; Handle ellipsis in template
         (let* ((p (car template))
                (p-vars (sc:collect-pattern-vars p literals ellipsis))
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
                          (sc:expand-syntax p (append iter-bindings bindings) context meta-env (+ depth 1) ellipsis literals suffix)))
                      (iota len))))
           (append new-template-list (sc:expand-syntax (cddr template) bindings context meta-env depth ellipsis literals suffix)))
         (cons (sc:expand-syntax (car template) bindings context meta-env depth ellipsis literals suffix)
               (sc:expand-syntax (cdr template) bindings context meta-env depth ellipsis literals suffix))))
    (else template)))

;;=============================================================================
;; 6. Quasisyntax & Transformation
;;=============================================================================

(define (sc:extract-quasisyntax x depth literals)
  (cond
    ((pair? x)
     (cond
       ((and (eq? (car x) 'quasisyntax) (pair? (cdr x)) (null? (cddr x)))
        (let ((res (sc:extract-quasisyntax (cadr x) (+ depth 1) literals)))
          (cons `(quasisyntax ,(car res)) (cdr res))))

       ((and (eq? (car x) 'unsyntax) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (string->symbol (string-append "unsyntax." (number->string (begin (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1)) sc:*syntax-temp-counter*))))))
              (cons (list '**splice-unsyntax** tmp) (list (list tmp (cadr x)))))
            (let ((res (sc:extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax ,(car res)) (cdr res)))))

       ((and (eq? (car x) 'unsyntax-splicing) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            (let ((tmp (string->symbol (string-append "unsyntax-splicing." (number->string (begin (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1)) sc:*syntax-temp-counter*))))))
              (cons (list '**splice-unsyntax-splicing** tmp) (list (list (list tmp '...) (cadr x)))))
            (let ((res (sc:extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax-splicing ,(car res)) (cdr res)))))

       (else
        (let ((car-res (sc:extract-quasisyntax (car x) depth literals))
              (cdr-res (sc:extract-quasisyntax (cdr x) depth literals)))
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
     (let ((res (sc:extract-quasisyntax (vector->list x) depth literals)))
       (cons (list->vector (car res)) (cdr res))))
    (else (cons x '()))))

(define (sc:prepare-eval-expr expr literals meta-env bindings context)
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
          `(let ((suffix (mc:fresh-suffix)))
             (sc:expand-syntax ',(cadr x) sc:*current-syntax-bindings* ',context sc:*current-syntax-meta-env* 0 '... ',literals suffix)))
         ((eq? (car x) 'quasisyntax)
          (let ((res (sc:extract-quasisyntax (cadr x) 0 literals)))
            (let ((new-tmpl (car res))
                  (bindings (cdr res)))
              (if (null? bindings)
                  `(let ((suffix (mc:fresh-suffix)))
                     (sc:expand-syntax ',new-tmpl sc:*current-syntax-bindings* ',context sc:*current-syntax-meta-env* 0 '... ',literals suffix))
                  (loop `(with-syntax ,bindings (syntax ,new-tmpl)))))))
         ((eq? (car x) 'syntax-case)
          (let ((input (loop (cadr x)))
                (lits (caddr x))
                (clauses-expr (cons 'list (map (lambda (c)
                                                 (let ((pat (car c))
                                                       (fender (if (null? (cddr c)) #t (cadr c)))
                                                       (output (if (null? (cddr c)) (cadr c) (caddr c)))
                                                       (new-lits (append (caddr x) literals)))
                                                   `(list ',pat
                                                          (lambda () ,(sc:prepare-eval-expr fender new-lits meta-env bindings context))
                                                          (lambda () ,(sc:prepare-eval-expr output new-lits meta-env bindings context)))))
                                               (cdddr x)))))
            `(sc:expand-syntax-case ,input ',lits ,clauses-expr (interaction-environment))))
          ((eq? (car x) 'with-syntax)
           (let ((b-specs (cadr x))
                 (body (cddr x)))
             `(sc:expand-with-syntax (list 'with-syntax (list ,@(map (lambda (s) `(list ',(car s) ,(loop (cadr s)))) b-specs)) (lambda () ,(loop (cons 'begin body)))) (interaction-environment))))
         ((eq? (car x) 'quote) x)
         (else (cons (loop (car x)) (loop (cdr x))))))
      (else x))))

;;=============================================================================
;; 7. Macro Expansion Entry Points
;;=============================================================================

(define (generate-temporaries l)
  (let ((lst (sc:syntax->list l)))
    (map (lambda (x)
           (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1))
           (sc:make-syntax-object (string->symbol (string-append "temp." (number->string sc:*syntax-temp-counter*))) '()))
         lst)))

(define (sc:expand-with-syntax expr env)
  (let ((bindings (cadr expr))
        (body-thunk (caddr expr)))
    (sc:expand-syntax-case (sc:make-syntax-object (map cadr bindings) '())
                        '()
                        (list (list (map car bindings) (lambda () #t) body-thunk))
                        env)))

(define (sc:expand-syntax-case input literals clauses env)
  (let ((ellipsis '...))
    (let loop ((clauses clauses))
      (if (null? clauses)
          (syntax-violation 'syntax-case "no matching pattern" input)
          (let* ((clause (car clauses))
                 (pattern (car clause))
                 (fender (cadr clause))
                 (output (caddr clause))
                 (m (sc:syntax-case-match literals pattern input ellipsis)))
            (if m
                (let ((old-bindings sc:*current-syntax-bindings*)
                      (old-meta sc:*current-syntax-meta-env*)
                      (new-meta (sc:syntax-depth-map pattern literals ellipsis 0)))
                  (set! sc:*current-syntax-bindings* (append m old-bindings))
                  (set! sc:*current-syntax-meta-env* (append new-meta old-meta))
                  (let ((matched? (sc:eval-transformer-expr fender literals pattern sc:*current-syntax-bindings* env)))
                    (if matched?
                        (let ((res (sc:eval-transformer-expr output literals pattern sc:*current-syntax-bindings* env)))
                          (set! sc:*current-syntax-bindings* old-bindings)
                          (set! sc:*current-syntax-meta-env* old-meta)
                          res)
                        (begin
                          (set! sc:*current-syntax-bindings* old-bindings)
                          (set! sc:*current-syntax-meta-env* old-meta)
                          (loop (cdr clauses))))))
                (loop (cdr clauses))))))))

(define (sc:eval-transformer-expr expr literals pattern bindings env)
  (if (procedure? expr)
      (expr)
      (if (eq? expr #t) #t
          (eval (sc:prepare-eval-expr expr literals sc:*current-syntax-meta-env* bindings '())
                (if (null? env) (interaction-environment) env)))))
