;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R6RS/R7RS-compatible macro expansion system.
;;
;; This module implements a hygienic macro expansion system that supports:
;; - syntax-rules (pattern-based macros)
;; - syntax-case (procedural macros)
;; - identifier-syntax and variable-transformers
;; - let-syntax, letrec-syntax, let*-syntax for local bindings
;; - R7RS-style libraries/modules (via define-module and import-module)
;; - Proper hygiene through alpha-renaming and context tracking

;;=============================================================================
;; SECTION 1: Globals & State
;;=============================================================================

;; Tracks all renamed identifiers: alias -> (original . context)
(define *rename-env* (make-eq-hashtable))

;; Memoization cache for resolve-core-form (cleared per macroexpand)
(define *resolve-cache* (make-eq-hashtable))

;; Parameter holding the current expansion context (m-env s-env r-env marks)
(define *current-context* (make-parameter #f))

;; Temporary variable for passing values through eval during module processing
(define _eval_temp_value.145bed32-69c0-4df2-8c06-89f53ab9907f (make-parameter #f))

;; Registry mapping module names to records: ((name . record) ...)
;; Record structure: ((exports . rename-map) . bindings)
(define current-module-registry (make-parameter '()))

(define current-module-strict-mode (make-parameter #t))

;;=============================================================================
;; SECTION 2: Identifier & Environment Resolution
;;=============================================================================

;; Register a renamed identifier in the global rename environment.
(define (register-renamed! alias original context)
  (hashtable-set! *rename-env* alias (cons original context)))

;; Unwrap a lazy environment reference used by letrec-syntax.
(define (unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise)) (or (cadr env) '()) env))

;; Resolve an identifier to its original symbol and context or just symbol.
;; Uses path compression: when a rename has no context and chains to another
;; rename, we flatten the chain so subsequent lookups are O(1).
(define (resolve-identifier id)
  (if (symbol? id)
      (let ((entry (hashtable-ref *rename-env* id #f)))
        (if entry
            (let ((original (car entry)) (context (cdr entry)))
              (if context
                  (cons context original)
                  ;; No context — follow chain with path compression
                  (let ((final (resolve-identifier original)))
                    (when (not (eq? final original))
                      (set-car! entry final))
                    final)))
            id))
      id))

;; Resolve a variable reference for code generation.
(define (resolve-variable id m-env s-env r-env marks)
  (let ((local (assq id r-env)))
    (if local
        (cdr local)
        (let ((entry (hashtable-ref *rename-env* id #f)))
          (if entry
              (let ((original (car entry)) (context (cdr entry)))
                (if context
                    (let ((c-m (car context)) (c-s (cadr context)) (c-r (caddr context)) (c-marks (cadddr context)))
                      (resolve-variable original c-m c-s c-r c-marks))
                    id))
              id)))))

;; Create a renamed symbol from an original name and suffix.
(define (rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

;; Look up a macro transformer by name in local and global environments.
(define (lookup-macro name env)
  (let ((env (unwrap-env env)))
    (let ((local-pair (assq name env)))
      (if local-pair
          (cdr local-pair)
          (let ((resolved (resolve-identifier name)))
            (if (pair? resolved)
                (let ((context (car resolved)) (original (cdr resolved)))
                  (lookup-macro original (unwrap-env (car context))))
                (let ((local-pair-resolved (assq resolved env)))
                  (if local-pair-resolved
                      (cdr local-pair-resolved)
                      (and (environment-macro-contains? resolved)
                           (let ((transformer (environment-macro-ref resolved)))
                             (and (not (eq? transformer 'builtin)) transformer)))))))))))

;; Resolve a symbol to its core form name, or #f if shadowed.
;; Memoized: caches results for symbols with rename entries.
(define (resolve-core-form sym shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((cached (hashtable-ref *resolve-cache* sym 'miss)))
        (if (not (eq? cached 'miss))
            cached
            (let ((result (resolve-core-form-slow sym shadowed-env)))
              (when (hashtable-ref *rename-env* sym #f)
                (hashtable-set! *resolve-cache* sym result))
              result)))))

(define (resolve-core-form-slow sym shadowed-env)
  (let ((resolved (resolve-identifier sym)))
    (if (pair? resolved)
        (let ((context (car resolved)) (original (cdr resolved)))
          (resolve-core-form original (unwrap-env (cadr context))))
        (if (memq resolved (unwrap-env shadowed-env)) #f resolved))))

;; Check if a symbol refers to a specific core form.
(define (core-form? sym name shadowed-env)
  (eq? (resolve-core-form sym shadowed-env) name))

;;=============================================================================
;; SECTION 3: Module System Helpers
;;=============================================================================

;; Register a module in the global registry.
(define (register-module! name exports rename-map bindings)
  (current-module-registry (cons (cons name (cons (cons exports rename-map) bindings)) (current-module-registry))))

(define (module-name->string name)
  (string-join (map symbol->string name) "."))

(define (mangle-name mod-name sym)
  (string->symbol (string-append (module-name->string mod-name) "`" (symbol->string sym))))

(define (lookup-module name)
  (let ((entry (assoc name (current-module-registry)))) (and entry (cdr entry))))

;; Extract and process export specifications.
(define (process-export-specs specs)
  (let loop ((specs specs) (exports '()) (renames '()))
    (if (null? specs)
        (cons (reverse exports) (reverse renames))
        (let ((spec (car specs)))
          (if (and (pair? spec) (eq? (car spec) 'rename))
              (loop (cdr specs) (cons (caddr spec) exports) (cons (cons (cadr spec) (caddr spec)) renames))
              (loop (cdr specs) (cons spec exports) renames))))))

(define (extract-library-name import-set)
  (if (and (pair? import-set) (memq (car import-set) '(only except rename prefix)))
      (extract-library-name (cadr import-set))
      import-set))

(define (apply-import-spec spec bindings)
  (if (not (pair? spec))
      bindings
      (case (car spec)
        ((only)
         (let ((ids (cddr spec)) (inner (apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (memq (car b) ids)) inner)))
        ((except)
         (let ((ids (cddr spec)) (inner (apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (not (memq (car b) ids))) inner)))
        ((rename)
         (let ((renames (cddr spec)) (inner (apply-import-spec (cadr spec) bindings)))
           (map (lambda (b) (let ((rename (assq (car b) renames))) (if rename (cons (cadr rename) (cdr b)) b))) inner)))
        ((prefix)
         (let ((pfx (caddr spec)) (inner (apply-import-spec (cadr spec) bindings)))
           (map (lambda (b) (cons (string->symbol (string-append (symbol->string pfx) (symbol->string (car b)))) (cdr b))) inner)))
        (else bindings))))

(define (process-import-set import-set)
  (let ((lib-name (extract-library-name import-set)))
    (load-module lib-name)
    (let ((record (lookup-module lib-name)))
      (unless record (error "Module not found" lib-name))
      (let* ((export-info (car record)) 
             (exports (car export-info)) 
             (rename-map (cdr export-info)) 
             (bindings (cdr record))
             (renamed-bindings 
               (map (lambda (b)
                      (let ((rename (assq (car b) rename-map)))
                        (if rename (cons (cdr rename) (cdr b)) b)))
                    bindings))
             (exported (filter (lambda (b) (memq (car b) exports)) renamed-bindings)))
        (apply-import-spec import-set exported)))))

(define (extract-module-defined-ids forms)
  (let loop ((forms forms) (ids '()))
    (if (null? forms)
        (reverse ids)
        (let ((form (car forms)))
          (loop (cdr forms) (if (and (pair? form) (eq? (car form) 'define))
                                (let ((pattern (cadr form))) (cons (if (pair? pattern) (car pattern) pattern) ids))
                                ids))))))

(define (filter-runtime-forms forms)
  (filter (lambda (f) (not (and (pair? f) (eq? (car f) 'define-syntax)))) forms))

(define (extract-macro-defs forms m-env s-env r-env marks)
  (let loop ((forms forms) (defs '()))
    (if (null? forms)
        (reverse defs)
        (let ((form (car forms)))
          (loop (cdr forms) (if (and (pair? form) (eq? (car form) 'define-syntax))
                                (cons (cons (cadr form) (make-macro-binding (parse-transformer (caddr form) (list m-env s-env r-env marks)))) defs)
                                defs))))))

(define (inject-binding! name value)
  (_eval_temp_value.145bed32-69c0-4df2-8c06-89f53ab9907f value)
  (core-eval `(define ,name (_eval_temp_value.145bed32-69c0-4df2-8c06-89f53ab9907f)) (current-environment)))

;; Collect all free symbol references from expanded code.
;; Returns a list of symbols that appear in variable-reference position,
;; excluding those bound by lambda, let, or letrec* in the code.
(define (collect-referenced-ids expr)
  (let ((seen (make-eq-hashtable)))
    (let walk ((expr expr) (bound '()))
      (cond
        ((symbol? expr)
         (unless (or (memq expr bound) (hashtable-contains? seen expr))
           (hashtable-set! seen expr #t)))
        ((not (pair? expr)) (unspecified))
        (else
         (let ((head (car expr)))
           (cond
             ((eq? head 'quote) (unspecified))
             ((eq? head 'lambda)
              (let* ((params (cadr expr))
                     (body (cddr expr))
                     (new-bound (append (get-param-names params) bound)))
                (for-each (lambda (e) (walk e new-bound)) body)))
             ((or (eq? head 'let) (eq? head 'letrec*))
              (let* ((bindings (cadr expr))
                     (body (cddr expr))
                     (vars (map car bindings))
                     (vals (map cadr bindings))
                     (inner-bound (append vars bound)))
                (if (eq? head 'let)
                    (for-each (lambda (v) (walk v bound)) vals)
                    (for-each (lambda (v) (walk v inner-bound)) vals))
                (for-each (lambda (e) (walk e inner-bound)) body)))
             ((eq? head 'define)
              (let ((pattern (cadr expr)))
                (let ((name (if (pair? pattern) (car pattern) pattern)))
                  (for-each (lambda (e) (walk e bound)) (cddr expr)))))
             ((eq? head 'set!)
              (walk (caddr expr) bound))
             ((eq? head 'if)
              (for-each (lambda (e) (walk e bound)) (cdr expr)))
             ((eq? head 'begin)
              (for-each (lambda (e) (walk e bound)) (cdr expr)))
             (else
              (for-each (lambda (e) (walk e bound)) expr)))))))
    (map car (hashtable->alist seen))))

;;=============================================================================
;; SECTION 4: Macro Transformers & Helpers
;;=============================================================================

(define (macro-binding? val) (and (pair? val) (eq? (car val) ':macro)))
(define (make-macro-binding trans) (cons ':macro trans))
(define (unwrap-macro-binding val) (cdr val))

;; Construct a begin expression, flattening nested begins and handling empty/single cases.
(define (make-seq exprs)
  (let ((flat (flatten-begins exprs)))
    (cond ((null? flat) '(begin))
          ((null? (cdr flat)) (car flat))
          (else `(begin ,@flat)))))

;; Invoke a macro transformer with current lexical context captured.
(define (call-transformer transformer expr m-env s-env r-env marks)
  (with-parameter ((*current-context* (list m-env s-env r-env marks)))
    (syntax->datum (if (variable-transformer? transformer)
                       ((variable-transformer-procedure transformer) expr)
                       (transformer expr)))))

;; Create a syntax-rules transformer.
(define (make-syntax-rules-transformer form captured-context)
  (let* ((has-ellipsis? (symbol? (car form)))
         (ellipsis-in (if has-ellipsis? (car form) '...))
         (literals (if has-ellipsis? (cadr form) (car form)))
         (rules (if has-ellipsis? (cddr form) (cdr form)))
         (ellipsis (if (memq ellipsis-in literals) (gensym "ellipsis") ellipsis-in))
         ;; Pre-resolve all pattern literals to their core forms (done once at creation)
         (def-s-env (cadr captured-context))
         (resolved-literals (map (lambda (lit) (cons lit (resolve-core-form lit def-s-env))) literals))
         ;; Pre-compute meta-env for each rule: (pattern template) -> (pattern template meta-env)
         (rules-with-meta (map (lambda (rule)
                                 (list (car rule) (cadr rule)
                                       (analyze-pattern (car rule) literals ellipsis 0)))
                               rules)))
    (lambda (expr)
      (let* ((suffix (fresh-suffix))
             (renamer (lambda (sym)
                        (if (eq? sym '...) '...
                            (let ((new-sym (rename-symbol sym suffix)))
                              (register-renamed! new-sym sym captured-context)
                              new-sym))))
             (literal=? (lambda (p-lit input)
                          (let* ((use-ctx (or (*current-context*) captured-context))
                                 (resolved-p (let ((e (assq p-lit resolved-literals))) (and e (cdr e))))
                                 (resolved-i (resolve-core-form input (cadr use-ctx))))
                            (and resolved-p resolved-i (eq? resolved-p resolved-i))))))
        (apply-syntax-rules literals rules-with-meta expr renamer ellipsis literal=?)))))

;; Parse a transformer specification (syntax-rules, lambda, etc.)
(define (parse-transformer spec context)
  (let ((head (if (pair? spec) (car spec) #f)))
    (cond
      ((and (pair? spec) (core-form? (car spec) 'syntax-rules (cadr context)))
       (make-syntax-rules-transformer (cdr spec) context))
      ((and (pair? spec) (core-form? (car spec) 'lambda (cadr context)))
       (let ((m-env (car context)) (s-env (cadr context)) (r-env (caddr context)) (marks (cadddr context)))
         (let ((new-marks (cons (fresh-mark) marks)))
           (lambda (expr)
             (let ((input (make-syntax-object expr (list m-env s-env r-env marks))))
               (let ((body (prepare-eval-expr `((lambda ,(cadr spec) ,@(cddr spec)) ',input) '() '() '() (list m-env s-env r-env new-marks))))
                 (syntax->datum (core-eval body (current-environment)))))))))
      ((and (pair? spec) (core-form? (car spec) 'make-variable-transformer (cadr context)))
       (make-variable-transformer (parse-transformer (cadr spec) context)))
      ((and (pair? spec) (core-form? (car spec) 'identifier-syntax (cadr context)))
       (let ((args (cdr spec)))
         (if (= (length args) 2)
             (let* ((c1 (car args)) (c2 (cadr args)) (t1 (cadr c1)) (t2 (cadr c2)))
                (parse-transformer
                 `(make-variable-transformer
                   (lambda (x) (syntax-case x (set!)
                                 ((set! _ val) (syntax ,t2))
                                 ((_ . rest) (syntax (,t1 . rest)))
                                 (_ (syntax ,t1))))) context))
             (parse-transformer `(lambda (x) (syntax-case x () ((_ . rest) (syntax (,(car args) . rest))) (_ (syntax ,(car args))))) context))))
      ((and head (symbol? head) (let ((m-env (unwrap-env (car context)))) (lookup-macro head m-env)))
       (let* ((m-env (unwrap-env (car context)))
              (s-env (cadr context))
              (r-env (caddr context))
              (marks (cadddr context))
              (transformer (lookup-macro head m-env)))
         (parse-transformer (call-transformer transformer spec m-env s-env r-env marks) context)))
      (else (error "Unsupported macro transformer" spec)))))

;;=============================================================================
;; SECTION 5: Binding & Expression Helpers
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

(define (split-internal-defines body shadowed-env)
  (let loop ((body body) (defs '()))
    (if (null? body)
        (cons (reverse defs) '())
        (let ((first (car body)))
          (cond
            ((and (pair? first) (core-form? (car first) 'define shadowed-env))
             (loop (cdr body) (cons (internal-define->binding first) defs)))
            ((and (pair? first) (core-form? (car first) 'begin shadowed-env))
             (loop (append (cdr first) (cdr body)) defs))
            (else (cons (reverse defs) body)))))))

;;=============================================================================
;; SECTION 6: Expansion Handlers
;;=============================================================================

(define (expand expr m-env s-env r-env marks)
  (cond
      ((pair? expr)
       (let ((head (car expr)))
         (if (symbol? head)
             (let ((transformer (and (not (memq head s-env)) (lookup-macro head m-env))))
               (if transformer 
                   (expand (call-transformer transformer expr m-env s-env r-env marks) m-env s-env r-env marks)
                   (let ((core-sym (resolve-core-form head s-env)))
                     (let ((handler (and core-sym (lookup-builtin-handler core-sym))))
                       (if handler 
                           (handler expr m-env s-env r-env marks) 
                           (map-improper (lambda (x) (expand x m-env s-env r-env marks)) expr))))))
             (map-improper (lambda (x) (expand x m-env s-env r-env marks)) expr))))
      ((symbol? expr)
       (let ((transformer (and (not (memq expr s-env)) (lookup-macro expr m-env))))
         (if transformer
             (expand (call-transformer transformer expr m-env s-env r-env marks) m-env s-env r-env marks)
             (resolve-variable expr m-env s-env r-env marks))))
      (else expr)))

(define (expand-lambda expr m-env s-env r-env marks)
  (let* ((params (cadr expr)) (body (cddr expr))
         (p-names (get-param-names params))
         (new-p-names (map (lambda (p) (rename-symbol p (fresh-suffix))) p-names))
         (new-r-env (append (reverse (map cons p-names new-p-names)) r-env))
         (new-s-env (append p-names s-env))
         (new-params (reconstruct-params params new-p-names))
         (split (split-internal-defines body new-s-env))
         (i-defs (car split)) (r-body (cdr split)))
    (for-each (lambda (p np) (register-renamed! np p (list m-env new-s-env new-r-env marks))) p-names new-p-names)
    (if (null? i-defs)
        `(lambda ,new-params ,@(let ((body (flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env marks)) r-body))))
                               (if (null? body) '((begin)) body)))
        `(lambda ,new-params ,(expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env marks)))))

(define (expand-let expr m-env s-env r-env marks)
  (if (symbol? (cadr expr))
      (let ((name (cadr expr)) (bindings (caddr expr)) (body (cdddr expr)))
        (let ((vars (map car bindings)) (vals (map cadr bindings)))
          (expand `((letrec* ((,name (lambda ,vars ,@body))) ,name) ,@vals) m-env s-env r-env marks)))
      (let* ((bindings (cadr expr)) (body (cddr expr))
             (vars (map car bindings)) (vals (map cadr bindings))
             (expanded-vals (map (lambda (x) (expand x m-env s-env r-env marks)) vals))
             (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
             (new-r-env (append (reverse (map cons vars new-vars)) r-env))
             (new-s-env (append vars s-env))
             (new-bindings (map list new-vars expanded-vals))
             (split (split-internal-defines body new-s-env))
             (i-defs (car split)) (r-body (cdr split)))
        (for-each (lambda (v nv) (register-renamed! nv v (list m-env new-s-env new-r-env marks))) vars new-vars)
        (if (null? i-defs)
            `(let ,new-bindings ,@(let ((body (flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env marks)) r-body))))
                                   (if (null? body) '((begin)) body)))
            `(let ,new-bindings ,(expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env marks))))))

(define (expand-letrec* expr m-env s-env r-env marks)
  (let* ((bindings (cadr expr)) (body (cddr expr))
         (split (split-internal-defines body s-env))
         (i-defs (car split)) (r-body (cdr split))
         (all-bindings (append bindings i-defs))
         (vars (map car all-bindings)) (vals (map cadr all-bindings))
         (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
         (new-r-env (append (reverse (map cons vars new-vars)) r-env))
         (new-s-env (append vars s-env))
         (expanded-vals (map (lambda (x) (expand x m-env new-s-env new-r-env marks)) vals))
         (new-bindings (map list new-vars expanded-vals)))
    (for-each (lambda (v nv) (register-renamed! nv v (list m-env new-s-env new-r-env marks))) vars new-vars)
    `(letrec* ,new-bindings ,@(flatten-begins (map-improper (lambda (x) (expand x m-env new-s-env new-r-env marks)) r-body)))))

(define (expand-let* expr m-env s-env r-env marks)
  (let ((bindings (cadr expr)) (body (cddr expr)))
    (cond ((null? bindings) (expand `(let () ,@body) m-env s-env r-env marks))
          ((null? (cdr bindings)) (expand `(let (,(car bindings)) ,@body) m-env s-env r-env marks))
          (else (expand `(let (,(car bindings)) (let* ,(cdr bindings) ,@body)) m-env s-env r-env marks)))))

(define (expand-let*-syntax expr m-env s-env r-env marks)
  (let ((bindings (cadr expr)) (body (cddr expr)))
    (cond ((null? bindings) (expand `(let-syntax () ,@body) m-env s-env r-env marks))
          ((null? (cdr bindings)) (expand `(let-syntax (,(car bindings)) ,@body) m-env s-env r-env marks))
          (else (expand `(let-syntax (,(car bindings)) (let*-syntax ,(cdr bindings) ,@body)) m-env s-env r-env marks)))))

(define (expand-let-syntax expr m-env s-env r-env marks)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (ctx (list m-env s-env r-env marks))
         (transformers (map (lambda (b) (parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (make-seq (map-improper (lambda (x) (expand x new-m-env new-s-env r-env marks)) (cddr expr)))))

(define (expand-letrec-syntax expr m-env s-env r-env marks)
  (let* ((bindings (cadr expr)) (names (map car bindings))
         (env-promise (list 'promise #f)) (ctx (list env-promise s-env r-env marks))
         (transformers (map (lambda (b) (parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    (set-car! (cdr env-promise) new-m-env)
    (make-seq (map-improper (lambda (x) (expand x new-m-env new-s-env r-env marks)) (cddr expr)))))

(define (expand-define-syntax expr m-env s-env r-env marks)
  (environment-macro-set! (cadr expr) (parse-transformer (caddr expr) (list m-env s-env r-env marks))) (unspecified))

(define (expand-define-module expr m-env s-env r-env marks)
  (let* ((mod-name (cadr expr)) (decls (cddr expr)))
    (let loop ((decls decls) (exports '()) (imports '()) (body-forms '()))
      (if (null? decls)
          (let* ((export-info (process-export-specs (reverse exports)))
                 (export-names (car export-info)) (rename-map (cdr export-info))
                 (imported-bindings (apply append (map process-import-set (reverse imports)))))
            (register-module! mod-name export-names rename-map (append (eval-module-body imported-bindings (reverse body-forms) m-env s-env r-env marks mod-name) imported-bindings))
            (unspecified))
          (let ((decl (car decls)))
            (cond ((and (pair? decl) (eq? (car decl) 'export)) (loop (cdr decls) (append (reverse (cdr decl)) exports) imports body-forms))
                  ((and (pair? decl) (eq? (car decl) 'import)) (loop (cdr decls) exports (append (reverse (cdr decl)) imports) body-forms))
                  ((and (pair? decl) (eq? (car decl) 'begin)) (loop (cdr decls) exports imports (append (reverse (cdr decl)) body-forms)))
                  (else (loop (cdr decls) exports imports (cons decl body-forms)))))))))

(define (expand-import-module expr m-env s-env r-env marks)
  (for-each (lambda (b) (if (macro-binding? (cdr b)) (environment-macro-set! (car b) (unwrap-macro-binding (cdr b))) (inject-binding! (car b) (cdr b))))
            (apply append (map process-import-set (cdr expr)))) (unspecified))

(define (expand-set! expr m-env s-env r-env marks)
  (let* ((var (cadr expr)) (transformer (and (symbol? var) (not (memq var s-env)) (lookup-macro var m-env))))
    (if (and transformer (variable-transformer? transformer))
        (expand (call-transformer transformer expr m-env s-env r-env marks) m-env s-env r-env marks)
        (let* ((pair (assq var r-env)) (renamed (if pair (cdr pair) var)))
          `(set! ,renamed ,(expand (caddr expr) m-env s-env r-env marks))))))

(define (expand-define expr m-env s-env r-env marks)
  (let ((head (cadr expr)))
    (if (pair? head)
        (expand `(define ,(car head) (lambda ,(cdr head) ,@(cddr expr))) m-env s-env r-env marks)
        `(define ,head ,@(flatten-begins (map-improper (lambda (x) (expand x m-env s-env r-env marks)) (cddr expr)))))))

(define (expand-cond expr m-env s-env r-env marks)
  (let ((clauses (cdr expr)))
    (if (null? clauses) '(begin)
        (let ((clause (car clauses)) (rest (cdr clauses)))
          (cond ((core-form? (car clause) 'else s-env) 
                 (expand (make-seq (cdr clause)) m-env s-env r-env marks))
                ((and (pair? (cdr clause)) (core-form? (cadr clause) '=> s-env))
                 (let ((tmp (rename-symbol 'tmp (fresh-suffix))))
                   (expand `(let ((,tmp ,(car clause))) (if ,tmp (,(caddr clause) ,tmp) (cond ,@rest))) m-env s-env r-env marks)))
                (else 
                 (expand `(if ,(car clause) ,(make-seq (cdr clause)) (cond ,@rest)) m-env s-env r-env marks)))))))

(define (expand-and expr m-env s-env r-env marks)
  (let ((args (cdr expr)))
    (cond ((null? args) #t) ((null? (cdr args)) (expand (car args) m-env s-env r-env marks))
          (else (expand `(if ,(car args) (and ,@(cdr args)) #f) m-env s-env r-env marks)))))

(define (expand-or expr m-env s-env r-env marks)
  (let ((args (cdr expr)))
    (cond ((null? args) #f) ((null? (cdr args)) (expand (car args) m-env s-env r-env marks))
          (else (let ((tmp (rename-symbol 'tmp (fresh-suffix))))
                  (expand `(let ((,tmp ,(car args))) (if ,tmp ,tmp (or ,@(cdr args)))) m-env s-env r-env marks))))))

(define (expand-case expr m-env s-env r-env marks)
  (let ((val (cadr expr)) (clauses (cddr expr)) (tmp (rename-symbol 'tmp (fresh-suffix))))
    (expand `(let ((,tmp ,val))
               ,(let recur ((cs clauses))
                  (if (null? cs) '(begin)
                      (let ((c (car cs)) (r (cdr cs)))
                        (if (core-form? (car c) 'else s-env) (make-seq (cdr c))
                            `(if (memv ,tmp ',(car c)) ,(make-seq (cdr c)) ,(recur r))))))) m-env s-env r-env marks)))

(define (expand-if expr m-env s-env r-env marks)
  `(if ,(expand (cadr expr) m-env s-env r-env marks) ,(expand (caddr expr) m-env s-env r-env marks)
       ,@(map-improper (lambda (x) (expand x m-env s-env r-env marks)) (cdddr expr))))

(define (expand-begin expr m-env s-env r-env marks)
  (make-seq (map-improper (lambda (x) (expand x m-env s-env r-env marks)) (cdr expr))))

(define (expand-quote expr m-env s-env r-env marks) `(quote ,(strip-renames (cadr expr))))

(define (expand-quasiquote expr m-env s-env r-env marks) (expand (expand-qq-form (cadr expr)) m-env s-env r-env marks))

;;=============================================================================
;; SECTION 7: Expansion Engine
;;=============================================================================

(define (eval-module-body imported-bindings body-forms m-env s-env r-env marks mod-name)
  (let* ((macro-m (filter (lambda (b) (macro-binding? (cdr b))) imported-bindings))
         (runtime-i (filter (lambda (b) (not (macro-binding? (cdr b)))) imported-bindings))
         (inner-m (append (map (lambda (b) (cons (car b) (unwrap-macro-binding (cdr b)))) macro-m) m-env))
         (exp-forms (flatten-begins (map (lambda (f) (if (and (pair? f) (eq? (car f) 'define-syntax)) f (expand f inner-m s-env r-env marks))) body-forms)))
         (rt-forms (filter-runtime-forms exp-forms))
         (def-ids (extract-module-defined-ids rt-forms))
         (internal-m (map (lambda (id) (cons id (mangle-name mod-name id))) def-ids))
         (inner-r (append internal-m r-env))
         (env-promise (list 'promise #f))
         (macro-b (extract-macro-defs exp-forms env-promise s-env inner-r marks)))
    (set-car! (cdr env-promise) (append (map (lambda (b) (cons (car b) (unwrap-macro-binding (cdr b)))) macro-b) inner-m))
    ;; Strict mode: check for unimported identifier references
    (when (current-module-strict-mode)
      (let* ((refs (collect-referenced-ids (cons 'begin rt-forms)))
             (imported-ids (map car imported-bindings))
             (macro-ids (map car macro-b))
             (mangled-ids (map cdr internal-m))
             (known-ids (append imported-ids def-ids macro-ids mangled-ids)))
        (for-each
          (lambda (id)
            (unless (or (memq id known-ids)
                        (lookup-builtin-handler id)
                        (environment-macro-contains? id)
                        (environment-variable-contains? id))
              (assertion-violation 'define-module (format "unbound variable ~s in ~s" id mod-name))))
          refs)))
    (let* ((imp-defs (map (lambda (b) `(define ,(car b) ',(cdr b))) runtime-i))
           (wrapper `(lambda () ,@imp-defs ,@rt-forms (list ,@def-ids))))
      (let* ((proc (core-eval wrapper (current-environment))) (vals (proc)) (rt-bindings (map cons def-ids vals)))
        (for-each (lambda (p val) (inject-binding! (cdr p) val)) internal-m vals)
        (append rt-bindings macro-b)))))

;;=============================================================================
;; SECTION 8: Cleanup & Entry Point
;;=============================================================================

(define (strip-suffix str)
  (let ((len (string-length str)))
    (let loop ((i (- len 1)))
      (if (>= i 0) 
          (if (char=? (string-ref str i) #\.) 
              (if (< i (- len 1))
                  (let check-numeric ((j (+ i 1))) 
                    (if (and (< j len) 
                        (char-numeric? (string-ref str j))) 
                        (check-numeric (+ j 1)) 
                        (if (= j len) (strip-suffix (substring str 0 i)) str)))
                  str) 
              (loop (- i 1))) 
          str))))

(define (strip-renames expr)
  (let ((expr (syntax->datum expr)))
    (cond ((symbol? expr) (string->symbol (strip-suffix (symbol->string expr))))
          ((pair? expr) (cons (strip-renames (car expr)) (strip-renames (cdr expr))))
          ((vector? expr) (list->vector (map strip-renames (vector->list expr))))
          (else expr))))

(define (unrename-core expr context)
  (let ((s-env (if (and (pair? context) (pair? (cdr context))) (cadr context) '())))
    (let loop ((x expr))
      (cond ((symbol? x)
             (let ((stripped (string->symbol (strip-suffix (symbol->string x)))))
               (if (or (lookup-builtin-handler stripped)
                       (environment-macro-contains? stripped)
                       (environment-variable-contains? stripped))
                   (if (eq? (resolve-core-form x s-env) stripped)
                       stripped
                       x)
                   x)))
            ((pair? x) (cons (loop (car x)) (loop (cdr x))))
            ((vector? x) (list->vector (map loop (vector->list x))))
            (else x)))))

(define (macroexpand-1 expr)
  (cond ((pair? expr) (let* ((h (car expr)) (t (and (symbol? h) (lookup-macro h '())))) (if t (t expr) expr)))
        ((symbol? expr) (let ((t (lookup-macro expr '()))) (if t (t expr) expr))) (else expr)))

(define (macroexpand expr . opt)
  (set! *suffix-counter* 0) (set! *mark-counter* 0) (hashtable-clear! *rename-env*) (hashtable-clear! *resolve-cache*)
  (let ((res (expand expr '() '() '() '()))) (if (and (pair? opt) (eq? (car opt) 'strip)) (strip-renames res) res)))

(define *builtin-handlers* (make-eq-hashtable))

(define (lookup-builtin-handler id)
  (and (eq? (environment-macro-ref id) 'builtin)
       (hashtable-ref *builtin-handlers* id #f)))

;; register macro builtins in current environment (system environment)
(for-each (lambda (pair) (environment-macro-set! (car pair) 'builtin) (hashtable-set! *builtin-handlers* (car pair) (cdr pair)))
  (list
    (cons 'and expand-and)
    (cons 'begin expand-begin)
    (cons 'case expand-case)
    (cons 'cond expand-cond)
    (cons 'define expand-define)
    (cons 'define-module expand-define-module)
    (cons 'define-syntax expand-define-syntax)
    (cons 'if expand-if)
    (cons 'import-module expand-import-module)
    (cons 'lambda expand-lambda)
    (cons 'let expand-let)
    (cons 'let* expand-let*)
    (cons 'letrec expand-letrec*)
    (cons 'letrec* expand-letrec*)
    (cons 'letrec-syntax expand-letrec-syntax)
    (cons 'let*-syntax expand-let*-syntax)
    (cons 'let-syntax expand-let-syntax)
    (cons 'or expand-or)
    (cons 'quasiquote expand-quasiquote)
    (cons 'quote expand-quote)
    (cons 'set! expand-set!)))
  
