;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R6RS/R7RS-compatible macro expansion system for research purposes.
;;
;; This module provides the main macro expansion logic. 
;; Pattern matching and substitution are provided by syntax_rules.scm and syntax_case.scm.

(load "./syntax_rules.scm")
(load "./syntax_case.scm")
(load "./quasiquote.scm")
(load "./syntax_common.scm")

;;=============================================================================
;; SECTION 1: Globals & State
;;=============================================================================

(define *macro-env* '())
(define *rename-counter* 0)
(define *rename-env* '())
(define *current-context* (make-parameter #f))

(define (fresh-suffix)
  (set! *rename-counter* (+ *rename-counter* 1))
  (number->string *rename-counter*))

(define (register-renamed! alias original context)
  (set! *rename-env* (cons (list alias original context) *rename-env*)))

(define (register-macro! name transformer)
  (set! *macro-env* (cons (cons name transformer) *macro-env*)))

;;=============================================================================
;; SECTION 2: Utilities
;;=============================================================================

(define (make-seq exprs)
  (let ((flat (flatten-begins exprs)))
    (cond ((null? flat) '(begin))
          ((null? (cdr flat)) (car flat))
          (else `(begin ,@flat)))))

;;=============================================================================
;; SECTION 3: Identifier & Environment Resolution
;;=============================================================================

(define (resolve-identifier id)
  (if (symbol? id)
      (let ((entry (assq id *rename-env*)))
        (if entry
            (let ((original (cadr entry)) (context (caddr entry)))
              (if context (cons context original) (resolve-identifier original)))
            id))
      id))

(define (resolve-variable id m-env s-env r-env)
  (let ((local (assq id r-env)))
    (if local
        (cdr local)
        (let ((entry (assq id *rename-env*)))
          (if entry
              (let ((original (cadr entry)) (context (caddr entry)))
                (if context
                    (let ((c-m (car context)) (c-s (cadr context)) (c-r (caddr context)))
                      (resolve-variable original c-m c-s c-r))
                    id))
              id)))))

(define (rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

(define (unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise)) (or (cadr env) '()) env))

(define (lookup-macro name env)
  (let ((local-pair (assq name (unwrap-env env))))
    (if local-pair
        (cdr local-pair)
        (let ((resolved (resolve-identifier name)))
          (if (pair? resolved)
              (let ((context (car resolved)) (original (cdr resolved)))
                (lookup-macro original (unwrap-env (car context))))
              (let ((local-pair-resolved (assq resolved (unwrap-env env))))
                (if local-pair-resolved
                    (cdr local-pair-resolved)
                    (let ((global-pair (assq resolved *macro-env*)))
                      (and global-pair (cdr global-pair))))))))))

(define (core-form? sym name shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (resolve-identifier sym)))
        (let ((res (if (pair? resolved)
                       (let ((context (car resolved)) (original (cdr resolved)))
                         (core-form? original name (unwrap-env (cadr context))))
                       (and (not (memq resolved (unwrap-env shadowed-env))) (eq? resolved name)))))
          res))))

;;=============================================================================
;; SECTION 4: Macro Transformers
;;=============================================================================

(define (make-syntax-rules-transformer form captured-context)
  (let* ((has-ellipsis? (symbol? (car form)))
         (ellipsis-in (if has-ellipsis? (car form) '...))
         (literals (if has-ellipsis? (cadr form) (car form)))
         (rules (if has-ellipsis? (cddr form) (cdr form)))
         (ellipsis (if (memq ellipsis-in literals) (gensym "ellipsis") ellipsis-in)))
    (lambda (expr)
      (let* ((suffix (fresh-suffix))
             (renamer (lambda (sym)
                        (if (eq? sym '...)
                            '...
                            (let ((new-sym (rename-symbol sym suffix)))
                              (register-renamed! new-sym sym captured-context)
                              new-sym))))
             (literal=? (lambda (p-lit input)
                          (core-form? input p-lit (cadr (or (*current-context*) captured-context))))))
        (apply-syntax-rules literals rules expr renamer ellipsis literal=?)))))

(define (call-transformer transformer expr m-env s-env r-env)
  (parameterize ((*current-context* (list m-env s-env r-env)))
    (syntax->datum (if (variable-transformer? transformer)
                       ((variable-transformer-procedure transformer) expr)
                       (transformer expr)))))

(define (parse-transformer spec context)
  (let ((head (if (pair? spec) (car spec) #f)))
    (cond
      ((and head (symbol? head) (lookup-macro head (car context)))
       (parse-transformer ((lookup-macro head (car context)) spec) context))
      ((and (pair? spec) (core-form? (car spec) 'syntax-rules (cadr context)))
       (make-syntax-rules-transformer (cdr spec) context))
      ((and (pair? spec) (core-form? (car spec) 'lambda (cadr context)))
       (lambda (expr)
         (let ((input (make-syntax-object expr context)))
           (let ((body (prepare-eval-expr `((lambda ,(cadr spec) ,@(cddr spec)) ',input) '() '() '() context)))
             (syntax->datum (eval body (interaction-environment)))))))
      ((and (pair? spec) (core-form? (car spec) 'make-variable-transformer (cadr context)))
       (let ((proc (parse-transformer (cadr spec) context)))
         (make-variable-transformer proc)))
      ((and (pair? spec) (core-form? (car spec) 'identifier-syntax (cadr context)))
       (let ((args (cdr spec)))
         (if (= (length args) 2)
             ;; (identifier-syntax (id1 template1) ((set! id2 var) template2))
             (let* ((c1 (car args)) (c2 (cadr args))
                    (t1 (cadr c1))
                    (t2 (cadr c2)))
                (parse-transformer
                 `(make-variable-transformer
                   (lambda (x)
                     (syntax-case x (set!)
                       ((set! _ val) (syntax ,t2))
                       ((_ . rest) (syntax (,t1 . rest)))
                       (_ (syntax ,t1)))))
                 context))
             ;; (identifier-syntax template)
             (parse-transformer
              `(lambda (x)
                 (syntax-case x ()
                   ((_ . rest) (syntax (,(car args) . rest)))
                   (_ (syntax ,(car args)))))
              context))))
      (else (error "Only syntax-rules, lambda, make-variable-transformer and identifier-syntax are supported for macros" spec)))))

;;=============================================================================
;; SECTION 5: Binding Helpers
;;=============================================================================

(define (get-param-names params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (get-param-names (cdr params))))
        (else '())))

(define (reconstruct-params old-params new-names)
  (cond ((null? old-params) '())
        ((symbol? old-params) (car new-names))
        ((pair? old-params) (cons (car new-names) (reconstruct-params (cdr old-params) (cdr new-names))))
        (else '())))

(define (internal-define->binding expr)
  (let ((pattern (cadr expr)))
    (if (pair? pattern)
        (list (car pattern) `(lambda ,(cdr pattern) ,@(cddr expr)))
        (list pattern (caddr expr)))))

(define (extract-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (core-form? (car first) 'define shadowed-env))
           (cons (internal-define->binding first) (extract-internal-defines (cdr body) shadowed-env)))
          ((and (pair? first) (core-form? (car first) 'begin shadowed-env))
           (append (extract-internal-defines (cdr first) shadowed-env) (extract-internal-defines (cdr body) shadowed-env)))
          (else '())))))

(define (skip-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (core-form? (car first) 'define shadowed-env)) (skip-internal-defines (cdr body) shadowed-env))
          ((and (pair? first) (core-form? (car first) 'begin shadowed-env))
           (append (skip-internal-defines (cdr first) shadowed-env) (skip-internal-defines (cdr body) shadowed-env)))
          (else body)))))

;;=============================================================================
;; SECTION 6: Expansion Handlers
;;=============================================================================

(define (expand-lambda expr m-env s-env r-env)
  (let* ((params (cadr expr)) (body (cddr expr))
         (p-names (get-param-names params))
         (new-p-names (map (lambda (p) (rename-symbol p (fresh-suffix))) p-names))
         (new-r-env (append (map cons p-names new-p-names) r-env))
         (new-s-env (append p-names s-env))
         (new-params (reconstruct-params params new-p-names))
         (i-defs (extract-internal-defines body new-s-env))
         (r-body (skip-internal-defines body new-s-env)))
    (for-each (lambda (p np) (register-renamed! np p (list m-env new-s-env new-r-env))) p-names new-p-names)
    (if (null? i-defs)
        `(lambda ,new-params ,@(flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env)) body)))
        `(lambda ,new-params ,(expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env)))))

(define (expand-let expr m-env s-env r-env)
  (if (symbol? (cadr expr))
      (let ((name (cadr expr)) (bindings (caddr expr)) (body (cdddr expr)))
        (let ((vars (map car bindings)) (vals (map cadr bindings)))
          (expand `((letrec* ((,name (lambda ,vars ,@body))) ,name) ,@vals) m-env s-env r-env)))
      (let* ((bindings (cadr expr)) (body (cddr expr))
             (vars (map car bindings)) (vals (map cadr bindings))
             (expanded-vals (map (lambda (x) (expand x m-env s-env r-env)) vals))
             (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
             (new-r-env (append (map cons vars new-vars) r-env))
             (new-s-env (append vars s-env))
             (new-bindings (map list new-vars expanded-vals))
             (i-defs (extract-internal-defines body new-s-env))
             (r-body (skip-internal-defines body new-s-env)))
        (for-each (lambda (v nv) (register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
        (if (null? i-defs)
            `(let ,new-bindings ,@(flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env)) body)))
            `(let ,new-bindings ,(expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env))))))

(define (expand-letrec-star expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (body (cddr expr))
         (i-defs (extract-internal-defines body s-env))
         (r-body (skip-internal-defines body s-env))
         (all-bindings (append bindings i-defs))
         (vars (map car all-bindings)) (vals (map cadr all-bindings))
         (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
         (new-r-env (append (map cons vars new-vars) r-env))
         (new-s-env (append vars s-env))
         (expanded-vals (map (lambda (x) (expand x m-env new-s-env new-r-env)) vals))
         (new-bindings (map list new-vars expanded-vals)))
    (for-each (lambda (v nv) (register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
    `(let ,(map (lambda (v) (list v ''*undefined*)) new-vars)
       ,@(map (lambda (v val) `(set! ,v ,val)) new-vars expanded-vals)
       ,@(flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env)) r-body)))))

;;=============================================================================
;; SECTION 7: Expansion Engine
;;=============================================================================

(define (expand-define-syntax expr m-env s-env r-env)
  (register-macro! (cadr expr) (parse-transformer (caddr expr) (list m-env s-env r-env)))
  ''defined)

(define (expand-let-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (ctx (list m-env s-env r-env))
         (transformers (map (lambda (b) (parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (make-seq (map-improper (lambda (x) (expand x new-m-env new-s-env r-env)) (cddr expr)))))

(define (expand-letrec-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (env-promise (list 'promise #f))
         (ctx (list env-promise s-env r-env))
         (transformers (map (lambda (b) (parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (set-car! (cdr env-promise) new-m-env)
    (make-seq (map-improper (lambda (x) (expand x new-m-env new-s-env r-env)) (cddr expr)))))

(define (expand-let*-syntax expr m-env s-env r-env)
  (let ((bindings (cadr expr)) (body (cddr expr)))
    (if (null? bindings)
        (make-seq (map-improper (lambda (x) (expand x m-env s-env r-env)) body))
        (expand `(let-syntax (,(car bindings)) (let*-syntax ,(cdr bindings) ,@body)) m-env s-env r-env))))

(define (expand-let* expr m-env s-env r-env)
  (let ((bits (cadr expr)) (body (cddr expr)))
    (cond ((null? bits) (expand `(let () ,@body) m-env s-env r-env))
          ((null? (cdr bits)) (expand `(let (,(car bits)) ,@body) m-env s-env r-env))
          (else (expand `(let (,(car bits)) (let* ,(cdr bits) ,@body)) m-env s-env r-env)))))

(define (expand-letrec expr m-env s-env r-env)
  (expand `(letrec* ,@(cdr expr)) m-env s-env r-env))

(define (expand-set! expr m-env s-env r-env)
  (let* ((var (cadr expr))
         (transformer (and (symbol? var) (not (memq var s-env)) (lookup-macro var m-env))))
    (if (and transformer (variable-transformer? transformer))
        (expand (call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
        (let* ((pair (assq var r-env))
               (renamed (if pair (cdr pair) var)))
          `(set! ,renamed ,(expand (caddr expr) m-env s-env r-env))))))

(define (expand-if expr m-env s-env r-env)
  `(if ,(expand (cadr expr) m-env s-env r-env) ,(expand (caddr expr) m-env s-env r-env)
       ,@(map-improper (lambda (x) (expand x m-env s-env r-env)) (cdddr expr))))

(define (expand-cond expr m-env s-env r-env)
  (let ((clauses (cdr expr)))
    (if (null? clauses)
        '(begin)
        (let ((clause (car clauses)) (rest (cdr clauses)))
          (cond
            ((core-form? (car clause) 'else s-env)
             (expand (make-seq (cdr clause)) m-env s-env r-env))
            ((and (pair? (cdr clause)) (core-form? (cadr clause) '=> s-env))
             (let ((tmp (rename-symbol 'tmp (fresh-suffix))))
               (expand `(let ((,tmp ,(car clause)))
                          (if ,tmp
                              (,(caddr clause) ,tmp)
                              (cond ,@rest)))
                       m-env s-env r-env)))
            (else
             (expand `(if ,(car clause)
                          ,(make-seq (cdr clause))
                          (cond ,@rest))
                     m-env s-env r-env)))))))

(define (expand-and expr m-env s-env r-env)
  (let ((args (cdr expr)))
    (cond
      ((null? args) #t)
      ((null? (cdr args)) (expand (car args) m-env s-env r-env))
      (else
       (expand `(if ,(car args) (and ,@(cdr args)) #f) m-env s-env r-env)))))

(define (expand-or expr m-env s-env r-env)
  (let ((args (cdr expr)))
    (cond
      ((null? args) #f)
      ((null? (cdr args)) (expand (car args) m-env s-env r-env))
      (else
       (let ((tmp (rename-symbol 'tmp (fresh-suffix))))
         (expand `(let ((,tmp ,(car args)))
                    (if ,tmp ,tmp (or ,@(cdr args))))
                 m-env s-env r-env))))))

(define (expand-case expr m-env s-env r-env)
  (let ((val (cadr expr)) (clauses (cddr expr)))
    (let ((tmp (rename-symbol 'tmp (fresh-suffix))))
      (expand `(let ((,tmp ,val))
                 ,(let recur ((clauses clauses))
                    (if (null? clauses)
                        '(begin)
                        (let ((clause (car clauses)) (rest (cdr clauses)))
                          (cond
                            ((core-form? (car clause) 'else s-env)
                             (make-seq (cdr clause)))
                            (else
                             `(if (memv ,tmp ',(car clause))
                                  ,(make-seq (cdr clause))
                                  ,(recur rest))))))))
              m-env s-env r-env))))

(define (expand-define expr m-env s-env r-env)
  `(define ,(cadr expr) ,@(flatten-begins (map-improper (lambda (x) (expand x m-env s-env r-env)) (cddr expr)))))

(define (expand-begin expr m-env s-env r-env)
  (make-seq (map-improper (lambda (x) (expand x m-env s-env r-env)) (cdr expr))))

(define (expand-quote expr m-env s-env r-env) expr)

(define (expand-quasiquote expr m-env s-env r-env)
  (expand (qq-expand (cadr expr)) m-env s-env r-env))

(define (lookup-handler core-sym)
  (case core-sym
    ((define-syntax) expand-define-syntax)
    ((let-syntax) expand-let-syntax)
    ((letrec-syntax) expand-letrec-syntax)
    ((let*-syntax) expand-let*-syntax)
    ((lambda) expand-lambda)
    ((let) expand-let)
    ((let*) expand-let*)
    ((letrec*) expand-letrec-star)
    ((letrec) expand-letrec)
    ((set!) expand-set!)
    ((if) expand-if)
    ((cond) expand-cond)
    ((and) expand-and)
    ((or) expand-or)
    ((case) expand-case)
    ((define) expand-define)
    ((begin) expand-begin)
    ((quote) expand-quote)
    ((quasiquote) expand-quasiquote)
    (else #f)))

(define (resolve-core-form sym shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (resolve-identifier sym)))
        (if (pair? resolved)
            (let ((context (car resolved)) (original (cdr resolved)))
              (resolve-core-form original (unwrap-env (cadr context))))
            (if (memq resolved (unwrap-env shadowed-env))
                #f
                resolved)))))

(define (expand expr . args)
  (let* ((m-env (if (null? args) '() (car args)))
         (s-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
         (r-env (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
    (cond
      ((pair? expr)
       (let ((head (car expr)))
         (if (symbol? head)
              (let ((transformer (and (not (memq head s-env)) (lookup-macro head m-env))))
                (if transformer
                    (expand (call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
                    (let ((core-sym (resolve-core-form head s-env)))
                      (let ((handler (and core-sym (lookup-handler core-sym))))
                        (if handler
                            (handler expr m-env s-env r-env)
                            (map-improper (lambda (x) (expand x m-env s-env r-env)) expr))))))
              (map-improper (lambda (x) (expand x m-env s-env r-env)) expr))))
      ((symbol? expr)
       (let ((transformer (and (not (memq expr s-env)) (lookup-macro expr m-env))))
         (if transformer
             (expand (call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
             (resolve-variable expr m-env s-env r-env))))
      (else expr))))

;;=============================================================================
;; SECTION 8: Cleanup & Entry Point
;;=============================================================================

(define (strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            (if (and (not (null? suffix)) (every? char-numeric? suffix))
                (strip-suffix (list->string (reverse (cdr chars))))
                str)
            (loop (cdr chars) (cons (car chars) suffix))))))

(define (strip-renames expr)
  (let ((expr (syntax->datum expr)))
    (cond ((symbol? expr) (string->symbol (strip-suffix (symbol->string expr))))
          ((pair? expr) (cons (strip-renames (car expr)) (strip-renames (cdr expr))))
          ((vector? expr) (list->vector (map strip-renames (vector->list expr))))
          (else expr))))

(define (macroexpand-1 expr)
  (cond
    ((pair? expr)
     (let* ((head (car expr))
            (transformer (and (symbol? head) (lookup-macro head '()))))
       (if transformer
           (transformer expr)
           expr)))
    ((symbol? expr)
     (let ((transformer (lookup-macro expr '())))
       (if transformer
           (transformer expr)
           expr)))
    (else expr)))

(define (macroexpand expr . opt)
  (set! *rename-counter* 0)
  (set! *rename-env* '())
  (let ((res (expand expr '() '() '())))
    (if (and (pair? opt) (eq? (car opt) 'strip)) (strip-renames res) res)))


