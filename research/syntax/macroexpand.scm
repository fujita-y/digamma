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

(define mc:*macro-env* '())
(define mc:*rename-counter* 0)
(define mc:*rename-env* '())
(define mc:*current-context* (make-parameter #f))

(define (mc:fresh-suffix)
  (set! mc:*rename-counter* (+ mc:*rename-counter* 1))
  (number->string mc:*rename-counter*))

(define (mc:register-renamed! alias original context)
  (set! mc:*rename-env* (cons (list alias original context) mc:*rename-env*)))

(define (mc:register-macro! name transformer)
  (set! mc:*macro-env* (cons (cons name transformer) mc:*macro-env*)))

;;=============================================================================
;; SECTION 2: Utilities
;;=============================================================================

(define (mc:make-seq exprs)
  (let ((flat (flatten-begins exprs)))
    (cond ((null? flat) '(begin))
          ((null? (cdr flat)) (car flat))
          (else `(begin ,@flat)))))

;;=============================================================================
;; SECTION 3: Identifier & Environment Resolution
;;=============================================================================

(define (mc:resolve-identifier id)
  (if (symbol? id)
      (let ((entry (assq id mc:*rename-env*)))
        (if entry
            (let ((original (cadr entry)) (context (caddr entry)))
              (if context (cons context original) (mc:resolve-identifier original)))
            id))
      id))

(define (mc:resolve-variable id m-env s-env r-env)
  (let ((local (assq id r-env)))
    (if local
        (cdr local)
        (let ((entry (assq id mc:*rename-env*)))
          (if entry
              (let ((original (cadr entry)) (context (caddr entry)))
                (if context
                    (let ((c-m (car context)) (c-s (cadr context)) (c-r (caddr context)))
                      (mc:resolve-variable original c-m c-s c-r))
                    id))
              id)))))

(define (mc:rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

(define (mc:unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise)) (or (cadr env) '()) env))

(define (mc:lookup-macro name env)
  (let ((local-pair (assq name (mc:unwrap-env env))))
    (if local-pair
        (cdr local-pair)
        (let ((resolved (mc:resolve-identifier name)))
          (if (pair? resolved)
              (let ((context (car resolved)) (original (cdr resolved)))
                (mc:lookup-macro original (mc:unwrap-env (car context))))
              (let ((local-pair-resolved (assq resolved (mc:unwrap-env env))))
                (if local-pair-resolved
                    (cdr local-pair-resolved)
                    (let ((global-pair (assq resolved mc:*macro-env*)))
                      (and global-pair (cdr global-pair))))))))))

(define (mc:core-form? sym name shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (mc:resolve-identifier sym)))
        (let ((res (if (pair? resolved)
                       (let ((context (car resolved)) (original (cdr resolved)))
                         (mc:core-form? original name (mc:unwrap-env (cadr context))))
                       (and (not (memq resolved (mc:unwrap-env shadowed-env))) (eq? resolved name)))))
          res))))

;;=============================================================================
;; SECTION 4: Macro Transformers
;;=============================================================================

(define (mc:make-syntax-rules-transformer form captured-context)
  (let* ((has-ellipsis? (symbol? (car form)))
         (ellipsis-in (if has-ellipsis? (car form) '...))
         (literals (if has-ellipsis? (cadr form) (car form)))
         (rules (if has-ellipsis? (cddr form) (cdr form)))
         (ellipsis (if (memq ellipsis-in literals) (gensym "ellipsis") ellipsis-in)))
    (lambda (expr)
      (let* ((suffix (mc:fresh-suffix))
             (renamer (lambda (sym)
                        (if (eq? sym '...)
                            '...
                            (let ((new-sym (mc:rename-symbol sym suffix)))
                              (mc:register-renamed! new-sym sym captured-context)
                              new-sym))))
             (literal=? (lambda (p-lit input)
                          (mc:core-form? input p-lit (cadr (or (mc:*current-context*) captured-context))))))
        (sr:apply-syntax-rules literals rules expr renamer ellipsis literal=?)))))

(define (mc:call-transformer transformer expr m-env s-env r-env)
  (parameterize ((mc:*current-context* (list m-env s-env r-env)))
    (syntax->datum (if (sc:variable-transformer? transformer)
                       ((sc:variable-transformer-procedure transformer) expr)
                       (transformer expr)))))

(define (mc:parse-transformer spec context)
  (let ((head (if (pair? spec) (car spec) #f)))
    (cond
      ((and head (symbol? head) (mc:lookup-macro head (car context)))
       (mc:parse-transformer ((mc:lookup-macro head (car context)) spec) context))
      ((and (pair? spec) (mc:core-form? (car spec) 'syntax-rules (cadr context)))
       (mc:make-syntax-rules-transformer (cdr spec) context))
      ((and (pair? spec) (mc:core-form? (car spec) 'lambda (cadr context)))
       (lambda (expr)
         (let ((input (sc:make-syntax-object expr context)))
           (let ((body (sc:prepare-eval-expr `((lambda ,(cadr spec) ,@(cddr spec)) ',input) '() '() '() context)))
             (syntax->datum (eval body (interaction-environment)))))))
      ((and (pair? spec) (mc:core-form? (car spec) 'make-variable-transformer (cadr context)))
       (let ((proc (mc:parse-transformer (cadr spec) context)))
         (make-variable-transformer proc)))
      ((and (pair? spec) (mc:core-form? (car spec) 'identifier-syntax (cadr context)))
       (let ((args (cdr spec)))
         (if (= (length args) 2)
             ;; (identifier-syntax (id1 template1) ((set! id2 var) template2))
             (let* ((c1 (car args)) (c2 (cadr args))
                    (t1 (cadr c1))
                    (t2 (cadr c2)))
                (mc:parse-transformer
                 `(make-variable-transformer
                   (lambda (x)
                     (syntax-case x (set!)
                       ((set! _ val) (syntax ,t2))
                       ((_ . rest) (syntax (,t1 . rest)))
                       (_ (syntax ,t1)))))
                 context))
             ;; (identifier-syntax template)
             (mc:parse-transformer
              `(lambda (x)
                 (syntax-case x ()
                   ((_ . rest) (syntax (,(car args) . rest)))
                   (_ (syntax ,(car args)))))
              context))))
      (else (error "Only syntax-rules, lambda, make-variable-transformer and identifier-syntax are supported for macros" spec)))))

;;=============================================================================
;; SECTION 5: Binding Helpers
;;=============================================================================

(define (mc:get-param-names params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (mc:get-param-names (cdr params))))
        (else '())))

(define (mc:reconstruct-params old-params new-names)
  (cond ((null? old-params) '())
        ((symbol? old-params) (car new-names))
        ((pair? old-params) (cons (car new-names) (mc:reconstruct-params (cdr old-params) (cdr new-names))))
        (else '())))

(define (mc:internal-define->binding expr)
  (let ((pattern (cadr expr)))
    (if (pair? pattern)
        (list (car pattern) `(lambda ,(cdr pattern) ,@(cddr expr)))
        (list pattern (caddr expr)))))

(define (mc:extract-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (mc:core-form? (car first) 'define shadowed-env))
           (cons (mc:internal-define->binding first) (mc:extract-internal-defines (cdr body) shadowed-env)))
          ((and (pair? first) (mc:core-form? (car first) 'begin shadowed-env))
           (append (mc:extract-internal-defines (cdr first) shadowed-env) (mc:extract-internal-defines (cdr body) shadowed-env)))
          (else '())))))

(define (mc:skip-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (mc:core-form? (car first) 'define shadowed-env)) (mc:skip-internal-defines (cdr body) shadowed-env))
          ((and (pair? first) (mc:core-form? (car first) 'begin shadowed-env))
           (append (mc:skip-internal-defines (cdr first) shadowed-env) (mc:skip-internal-defines (cdr body) shadowed-env)))
          (else body)))))

;;=============================================================================
;; SECTION 6: Expansion Handlers
;;=============================================================================

(define (mc:expand-lambda expr m-env s-env r-env)
  (let* ((params (cadr expr)) (body (cddr expr))
         (p-names (mc:get-param-names params))
         (new-p-names (map (lambda (p) (mc:rename-symbol p (mc:fresh-suffix))) p-names))
         (new-r-env (append (map cons p-names new-p-names) r-env))
         (new-s-env (append p-names s-env))
         (new-params (mc:reconstruct-params params new-p-names))
         (i-defs (mc:extract-internal-defines body new-s-env))
         (r-body (mc:skip-internal-defines body new-s-env)))
    (for-each (lambda (p np) (mc:register-renamed! np p (list m-env new-s-env new-r-env))) p-names new-p-names)
    (if (null? i-defs)
        `(lambda ,new-params ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) body)))
        `(lambda ,new-params ,(mc:expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env)))))

(define (mc:expand-let expr m-env s-env r-env)
  (if (symbol? (cadr expr))
      (let ((name (cadr expr)) (bindings (caddr expr)) (body (cdddr expr)))
        (let ((vars (map car bindings)) (vals (map cadr bindings)))
          (mc:expand `((letrec* ((,name (lambda ,vars ,@body))) ,name) ,@vals) m-env s-env r-env)))
      (let* ((bindings (cadr expr)) (body (cddr expr))
             (vars (map car bindings)) (vals (map cadr bindings))
             (expanded-vals (map (lambda (x) (mc:expand x m-env s-env r-env)) vals))
             (new-vars (map (lambda (v) (mc:rename-symbol v (mc:fresh-suffix))) vars))
             (new-r-env (append (map cons vars new-vars) r-env))
             (new-s-env (append vars s-env))
             (new-bindings (map list new-vars expanded-vals))
             (i-defs (mc:extract-internal-defines body new-s-env))
             (r-body (mc:skip-internal-defines body new-s-env)))
        (for-each (lambda (v nv) (mc:register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
        (if (null? i-defs)
            `(let ,new-bindings ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) body)))
            `(let ,new-bindings ,(mc:expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env))))))

(define (mc:expand-letrec-star expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (body (cddr expr))
         (i-defs (mc:extract-internal-defines body s-env))
         (r-body (mc:skip-internal-defines body s-env))
         (all-bindings (append bindings i-defs))
         (vars (map car all-bindings)) (vals (map cadr all-bindings))
         (new-vars (map (lambda (v) (mc:rename-symbol v (mc:fresh-suffix))) vars))
         (new-r-env (append (map cons vars new-vars) r-env))
         (new-s-env (append vars s-env))
         (expanded-vals (map (lambda (x) (mc:expand x m-env new-s-env new-r-env)) vals))
         (new-bindings (map list new-vars expanded-vals)))
    (for-each (lambda (v nv) (mc:register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
    `(let ,(map (lambda (v) (list v ''*undefined*)) new-vars)
       ,@(map (lambda (v val) `(set! ,v ,val)) new-vars expanded-vals)
       ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) r-body)))))

;;=============================================================================
;; SECTION 7: Expansion Engine
;;=============================================================================

(define (mc:expand-define-syntax expr m-env s-env r-env)
  (mc:register-macro! (cadr expr) (mc:parse-transformer (caddr expr) (list m-env s-env r-env)))
  ''defined)

(define (mc:expand-let-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (ctx (list m-env s-env r-env))
         (transformers (map (lambda (b) (mc:parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (mc:make-seq (map-improper (lambda (x) (mc:expand x new-m-env new-s-env r-env)) (cddr expr)))))

(define (mc:expand-letrec-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (env-promise (list 'promise #f))
         (ctx (list env-promise s-env r-env))
         (transformers (map (lambda (b) (mc:parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (set-car! (cdr env-promise) new-m-env)
    (mc:make-seq (map-improper (lambda (x) (mc:expand x new-m-env new-s-env r-env)) (cddr expr)))))

(define (mc:expand-let*-syntax expr m-env s-env r-env)
  (let ((bindings (cadr expr)) (body (cddr expr)))
    (if (null? bindings)
        (mc:make-seq (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) body))
        (mc:expand `(let-syntax (,(car bindings)) (let*-syntax ,(cdr bindings) ,@body)) m-env s-env r-env))))

(define (mc:expand-let* expr m-env s-env r-env)
  (let ((bits (cadr expr)) (body (cddr expr)))
    (cond ((null? bits) (mc:expand `(let () ,@body) m-env s-env r-env))
          ((null? (cdr bits)) (mc:expand `(let (,(car bits)) ,@body) m-env s-env r-env))
          (else (mc:expand `(let (,(car bits)) (let* ,(cdr bits) ,@body)) m-env s-env r-env)))))

(define (mc:expand-letrec expr m-env s-env r-env)
  (mc:expand `(letrec* ,@(cdr expr)) m-env s-env r-env))

(define (mc:expand-set! expr m-env s-env r-env)
  (let* ((var (cadr expr))
         (transformer (and (symbol? var) (not (memq var s-env)) (mc:lookup-macro var m-env))))
    (if (and transformer (sc:variable-transformer? transformer))
        (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
        (let* ((pair (assq var r-env))
               (renamed (if pair (cdr pair) var)))
          `(set! ,renamed ,(mc:expand (caddr expr) m-env s-env r-env))))))

(define (mc:expand-if expr m-env s-env r-env)
  `(if ,(mc:expand (cadr expr) m-env s-env r-env) ,(mc:expand (caddr expr) m-env s-env r-env)
       ,@(map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cdddr expr))))

(define (mc:expand-cond expr m-env s-env r-env)
  (let ((clauses (cdr expr)))
    (if (null? clauses)
        '(begin)
        (let ((clause (car clauses)) (rest (cdr clauses)))
          (cond
            ((mc:core-form? (car clause) 'else s-env)
             (mc:expand (mc:make-seq (cdr clause)) m-env s-env r-env))
            ((and (pair? (cdr clause)) (mc:core-form? (cadr clause) '=> s-env))
             (let ((tmp (mc:rename-symbol 'tmp (mc:fresh-suffix))))
               (mc:expand `(let ((,tmp ,(car clause)))
                          (if ,tmp
                              (,(caddr clause) ,tmp)
                              (cond ,@rest)))
                       m-env s-env r-env)))
            (else
             (mc:expand `(if ,(car clause)
                          ,(mc:make-seq (cdr clause))
                          (cond ,@rest))
                     m-env s-env r-env)))))))

(define (mc:expand-and expr m-env s-env r-env)
  (let ((args (cdr expr)))
    (cond
      ((null? args) #t)
      ((null? (cdr args)) (mc:expand (car args) m-env s-env r-env))
      (else
       (mc:expand `(if ,(car args) (and ,@(cdr args)) #f) m-env s-env r-env)))))

(define (mc:expand-or expr m-env s-env r-env)
  (let ((args (cdr expr)))
    (cond
      ((null? args) #f)
      ((null? (cdr args)) (mc:expand (car args) m-env s-env r-env))
      (else
       (let ((tmp (mc:rename-symbol 'tmp (mc:fresh-suffix))))
         (mc:expand `(let ((,tmp ,(car args)))
                    (if ,tmp ,tmp (or ,@(cdr args))))
                 m-env s-env r-env))))))

(define (mc:expand-case expr m-env s-env r-env)
  (let ((val (cadr expr)) (clauses (cddr expr)))
    (let ((tmp (mc:rename-symbol 'tmp (mc:fresh-suffix))))
      (mc:expand `(let ((,tmp ,val))
                 ,(let recur ((clauses clauses))
                    (if (null? clauses)
                        '(begin)
                        (let ((clause (car clauses)) (rest (cdr clauses)))
                          (cond
                            ((mc:core-form? (car clause) 'else s-env)
                             (mc:make-seq (cdr clause)))
                            (else
                             `(if (memv ,tmp ',(car clause))
                                  ,(mc:make-seq (cdr clause))
                                  ,(recur rest))))))))
              m-env s-env r-env))))

(define (mc:expand-define expr m-env s-env r-env)
  `(define ,(cadr expr) ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cddr expr)))))

(define (mc:expand-begin expr m-env s-env r-env)
  (mc:make-seq (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cdr expr))))

(define (mc:expand-quote expr m-env s-env r-env)
  `(quote ,(mc:strip-renames (cadr expr))))

(define (mc:expand-quasiquote expr m-env s-env r-env)
  (mc:expand (qq:expand (cadr expr)) m-env s-env r-env))

(define (mc:lookup-handler core-sym)
  (case core-sym
    ((define-syntax) mc:expand-define-syntax)
    ((let-syntax) mc:expand-let-syntax)
    ((letrec-syntax) mc:expand-letrec-syntax)
    ((let*-syntax) mc:expand-let*-syntax)
    ((lambda) mc:expand-lambda)
    ((let) mc:expand-let)
    ((let*) mc:expand-let*)
    ((letrec*) mc:expand-letrec-star)
    ((letrec) mc:expand-letrec)
    ((set!) mc:expand-set!)
    ((if) mc:expand-if)
    ((cond) mc:expand-cond)
    ((and) mc:expand-and)
    ((or) mc:expand-or)
    ((case) mc:expand-case)
    ((define) mc:expand-define)
    ((begin) mc:expand-begin)
    ((quote) mc:expand-quote)
    ((quasiquote) mc:expand-quasiquote)
    (else #f)))

(define (mc:resolve-core-form sym shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (mc:resolve-identifier sym)))
        (if (pair? resolved)
            (let ((context (car resolved)) (original (cdr resolved)))
              (mc:resolve-core-form original (mc:unwrap-env (cadr context))))
            (if (memq resolved (mc:unwrap-env shadowed-env))
                #f
                resolved)))))

(define (mc:expand expr . args)
  (let* ((m-env (if (null? args) '() (car args)))
         (s-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
         (r-env (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
    (cond
      ((pair? expr)
       (let ((head (car expr)))
         (if (symbol? head)
              (let ((transformer (and (not (memq head s-env)) (mc:lookup-macro head m-env))))
                (if transformer
                    (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
                    (let ((core-sym (mc:resolve-core-form head s-env)))
                      (let ((handler (and core-sym (mc:lookup-handler core-sym))))
                        (if handler
                            (handler expr m-env s-env r-env)
                            (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) expr))))))
              (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) expr))))
      ((symbol? expr)
       (let ((transformer (and (not (memq expr s-env)) (mc:lookup-macro expr m-env))))
         (if transformer
             (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
             (mc:resolve-variable expr m-env s-env r-env))))
      (else expr))))

;;=============================================================================
;; SECTION 8: Cleanup & Entry Point
;;=============================================================================

(define (mc:strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            (if (and (not (null? suffix)) (every? char-numeric? suffix))
                (mc:strip-suffix (list->string (reverse (cdr chars))))
                str)
            (loop (cdr chars) (cons (car chars) suffix))))))

(define (mc:strip-renames expr)
  (let ((expr (syntax->datum expr)))
    (cond ((symbol? expr) (string->symbol (mc:strip-suffix (symbol->string expr))))
          ((pair? expr) (cons (mc:strip-renames (car expr)) (mc:strip-renames (cdr expr))))
          ((vector? expr) (list->vector (map mc:strip-renames (vector->list expr))))
          (else expr))))

(define (macroexpand-1 expr)
  (cond
    ((pair? expr)
     (let* ((head (car expr))
            (transformer (and (symbol? head) (mc:lookup-macro head '()))))
       (if transformer
           (transformer expr)
           expr)))
    ((symbol? expr)
     (let ((transformer (mc:lookup-macro expr '())))
       (if transformer
           (transformer expr)
           expr)))
    (else expr)))

(define (macroexpand expr . opt)
  (set! mc:*rename-counter* 0)
  (set! mc:*rename-env* '())
  (let ((res (mc:expand expr '() '() '())))
    (if (and (pair? opt) (eq? (car opt) 'strip)) (mc:strip-renames res) res)))


