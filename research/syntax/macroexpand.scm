;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R6RS/R7RS-compatible macro expansion system for research purposes.
;;
;; This module implements a hygienic macro expansion system that supports:
;; - syntax-rules (pattern-based macros)
;; - syntax-case (procedural macros with pattern matching)
;; - identifier-syntax and make-variable-transformer
;; - let-syntax, letrec-syntax, let*-syntax for local macro bindings
;; - Proper hygiene through alpha-renaming of bound identifiers
;;
;; The expansion process maintains three environments:
;; - m-env: macro environment (maps names to transformers)
;; - s-env: shadow environment (list of identifiers that shadow core forms)
;; - r-env: rename environment (maps original names to renamed identifiers)
;;
;; Pattern matching and substitution are provided by syntax_rules.scm and syntax_case.scm.

(load "./syntax_rules.scm")
(load "./syntax_case.scm")
(load "./quasiquote.scm")
(load "./syntax_common.scm")

;;=============================================================================
;; SECTION 1: Globals & State
;;=============================================================================
;;
;; Global state variables that track macro definitions and hygiene information
;; across the expansion process. These are reset at the start of each top-level
;; macroexpand call.

;; Association list of global macro definitions: ((name . transformer) ...)
(define mc:*macro-env* '())

;; Counter for generating unique suffixes to ensure hygiene during renaming
(define mc:*rename-counter* 0)

;; Tracks all renamed identifiers: ((alias original context) ...)
;; Used to resolve renamed identifiers back to their original bindings
(define mc:*rename-env* '())

;; Parameter holding the current expansion context (m-env s-env r-env)
;; Used during transformer invocation to capture lexical environment
(define mc:*current-context* (make-parameter #f))

;; Generate a fresh unique suffix for identifier renaming.
;; Each call increments the counter and returns a new numeric string.
(define (mc:fresh-suffix)
  (set! mc:*rename-counter* (+ mc:*rename-counter* 1))
  (number->string mc:*rename-counter*))

;; Register a renamed identifier in the global rename environment.
;; alias: the new renamed symbol (e.g., x.1)
;; original: the original symbol name (e.g., x)
;; context: the lexical context (m-env s-env r-env) where it was introduced
(define (mc:register-renamed! alias original context)
  (set! mc:*rename-env* (cons (list alias original context) mc:*rename-env*)))

;; Register a global macro definition.
;; Adds the (name . transformer) pair to the front of the macro environment.
(define (mc:register-macro! name transformer)
  (set! mc:*macro-env* (cons (cons name transformer) mc:*macro-env*)))

;; Module registry for define-module and import-module
;; Registry mapping module names to records: ((name . record) ...)
;; Each record is ((exports . rename-map) . bindings) where:
;;   exports:     list of exported identifier symbols (external names)
;;   rename-map:  alist of (internal-name . external-name) for renamed exports
;;   bindings:    alist of (identifier . value) pairs
(define mc:*modules* '())

;; Temporary variable for passing values through eval
(define mc:*temp-value* #f)

;;=============================================================================
;; SECTION 2: Utilities
;;=============================================================================
;;
;; Helper functions for expression manipulation and sequence construction.

;; Construct a begin expression from a list of expressions.
;; Flattens nested begins, returns single expression if only one,
;; or (begin) if empty.
(define (mc:make-seq exprs)
  (let ((flat (flatten-begins exprs)))
    (cond ((null? flat) '(begin))
          ((null? (cdr flat)) (car flat))
          (else `(begin ,@flat)))))

;;=============================================================================
;; SECTION 3: Identifier & Environment Resolution
;;=============================================================================
;;
;; Functions for resolving identifiers through the rename and macro environments.
;; These handle hygiene by tracking where identifiers were introduced and
;; looking them up in the appropriate lexical context.

;; Resolve an identifier to its binding information.
;; Returns either:
;; - The original symbol if not renamed
;; - A pair (context . original-name) if the identifier was introduced by a macro
;; This is used for hygiene checking, not for variable lookup.
(define (mc:resolve-identifier id)
  (if (symbol? id)
      (let ((entry (assq id mc:*rename-env*)))
        (if entry
            ;; Found in rename environment - get original name and context
            (let ((original (cadr entry)) (context (caddr entry)))
              (if context 
                  ;; Has a context - return (context . original) for hygiene lookup
                  (cons context original) 
                  ;; No context - recursively resolve the original
                  (mc:resolve-identifier original)))
            ;; Not renamed - return as-is
            id))
      id))

;; Resolve a variable reference to its renamed form for code generation.
;; Looks up the identifier in:
;; 1. Local rename environment (r-env) first
;; 2. Global rename environment (mc:*rename-env*) with context chasing
;; Returns the renamed symbol or the original if not found.
(define (mc:resolve-variable id m-env s-env r-env)
  (let ((local (assq id r-env)))
    (if local
        ;; Found in local rename env - return the renamed symbol
        (cdr local)
        (let ((entry (assq id mc:*rename-env*)))
          (if entry
              (let ((original (cadr entry)) (context (caddr entry)))
                (if context
                    ;; Chase through the context to find the proper binding
                    (let ((c-m (car context)) (c-s (cadr context)) (c-r (caddr context)))
                      (mc:resolve-variable original c-m c-s c-r))
                    ;; No context - return original
                    id))
              ;; Not found - return as-is
              id)))))

;; Create a renamed symbol by appending a suffix with a dot separator.
;; Example: (mc:rename-symbol 'x "1") => x.1
(define (mc:rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

;; Unwrap a lazy environment reference.
;; letrec-syntax uses ('promise #f) as a placeholder that gets filled in later.
;; This unwraps the promise to get the actual environment or returns '() if not yet set.
(define (mc:unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise)) (or (cadr env) '()) env))

;; Look up a macro transformer by name.
;; Search order:
;; 1. Current local macro environment
;; 2. If the name is renamed, look up in the context where it was introduced
;; 3. Global macro environment
;; Returns the transformer procedure or #f if not found.
(define (mc:lookup-macro name env)
  (let ((local-pair (assq name (mc:unwrap-env env))))
    (if local-pair
        ;; Found in local macro environment
        (cdr local-pair)
        (let ((resolved (mc:resolve-identifier name)))
          (if (pair? resolved)
              ;; Name was renamed - look up in the context where it was introduced
              (let ((context (car resolved)) (original (cdr resolved)))
                (mc:lookup-macro original (mc:unwrap-env (car context))))
              ;; Try resolved name in current env, then fall back to global
              (let ((local-pair-resolved (assq resolved (mc:unwrap-env env))))
                (if local-pair-resolved
                    (cdr local-pair-resolved)
                    ;; Look up in global macro environment
                    (let ((global-pair (assq resolved mc:*macro-env*)))
                      (and global-pair (cdr global-pair))))))))))

;; Check if a symbol refers to a specific core form.
;; A core form is one that is not shadowed by a local binding.
;; Handles renamed identifiers by checking in their original context.
;; sym: the symbol to check
;; name: the core form name (e.g., 'lambda, 'if)
;; shadowed-env: list of identifiers that shadow core forms in current scope
;; Returns #t if sym refers to the core form 'name'.
(define (mc:core-form? sym name shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (mc:resolve-identifier sym)))
        (let ((res (if (pair? resolved)
                       ;; Renamed - check in original context
                       (let ((context (car resolved)) (original (cdr resolved)))
                         (mc:core-form? original name (mc:unwrap-env (cadr context))))
                       ;; Not renamed - check if it's the core form and not shadowed
                       (and (not (memq resolved (mc:unwrap-env shadowed-env))) (eq? resolved name)))))
          res))))


;;=============================================================================
;; SECTION 2.5: Module System Helpers
;;=============================================================================
;;
;; Helper functions for the module system (define-module and import-module).
;; These implement the core logic for registering modules, processing exports
;; and imports, and managing the module registry.

;; Register a module in the global registry
(define (mc:register-module! name exports rename-map bindings)
  (set! mc:*modules* 
        (cons (cons name (cons (cons exports rename-map) bindings)) 
              mc:*modules*)))

;; Helper: Convert module name list to string
(define (mc:module-name->string name)
  (string-join (map symbol->string name) "."))

;; Helper: Mangle internal name with module prefix
(define (mc:mangle-name mod-name sym)
  (string->symbol 
   (string-append (mc:module-name->string mod-name) "%" (symbol->string sym))))

;; Lookup a module by name, returns ((exports . rename-map) . bindings) or #f
(define (mc:lookup-module name)
  (let ((entry (assoc name mc:*modules*)))
    (and entry (cdr entry))))

;; Generic scanner: extract and flatten contents of declarations by keyword
;; Returns concatenated (cdr ...) of all declarations matching the keyword
(define (mc:scan-decls keyword decls)
  (let loop ((decls decls) (result '()))
    (cond
      ((null? decls) (reverse result))
      ((and (pair? (car decls)) (eq? (caar decls) keyword))
       (loop (cdr decls) (append (reverse (cdar decls)) result)))
      (else
       (loop (cdr decls) result)))))

;; Convenience wrappers for specific declaration types
(define (mc:scan-exports decls) (mc:scan-decls 'export decls))
(define (mc:scan-imports decls) (mc:scan-decls 'import decls))
(define (mc:scan-begins decls)  (mc:scan-decls 'begin decls))

;; Process export specifications into (exports . rename-map)
;; Export specs can be: identifier or (rename internal-name external-name)
;; Returns (exported-names . rename-alist)
(define (mc:process-export-specs specs)
  (let loop ((specs specs) (exports '()) (renames '()))
    (if (null? specs)
        (cons (reverse exports) (reverse renames))
        (let ((spec (car specs)))
          (if (and (pair? spec) (eq? (car spec) 'rename))
              ;; (rename internal external)
              (let ((internal (cadr spec))
                    (external (caddr spec)))
                (loop (cdr specs) 
                      (cons external exports)
                      (cons (cons internal external) renames)))
              ;; Plain identifier
              (loop (cdr specs) (cons spec exports) renames))))))

;; Extract the base library name from an import set by unwrapping modifiers
(define (mc:extract-library-name import-set)
  (if (and (pair? import-set)
           (memq (car import-set) '(only except rename prefix)))
      (mc:extract-library-name (cadr import-set))
      import-set))

;; Apply import modifiers to transform bindings
;; spec: import specification (library name or modifier form)
;; bindings: alist of (id . value) to transform
;; Returns transformed bindings alist
(define (mc:apply-import-spec spec bindings)
  (if (not (pair? spec))
      bindings
      (case (car spec)
        ((only)
         (let ((ids (cddr spec))
               (inner (mc:apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (memq (car b) ids)) inner)))
        
        ((except)
         (let ((ids (cddr spec))
               (inner (mc:apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (not (memq (car b) ids))) inner)))
        
        ((rename)
         (let ((renames (cddr spec))
               (inner (mc:apply-import-spec (cadr spec) bindings)))
           (map (lambda (b)
                  (let ((rename (assq (car b) renames)))
                    (if rename
                        (cons (cadr rename) (cdr b))
                        b)))
                inner)))
        
        ((prefix)
         (let ((pfx (caddr spec))
               (inner (mc:apply-import-spec (cadr spec) bindings)))
           (map (lambda (b)
                  (cons (string->symbol
                         (string-append (symbol->string pfx)
                                        (symbol->string (car b))))
                        (cdr b)))
                inner)))
        
        ;; Unrecognized form - treat as plain library name
        (else bindings))))

;; Process an import set: lookup library and apply modifiers
;; Returns filtered and transformed bindings alist
(define (mc:process-import-set import-set)
  (let* ((lib-name (mc:extract-library-name import-set))
         (record (mc:lookup-module lib-name)))
    (unless record
      (error "Module not found" lib-name))
    (let* ((export-info (car record))
           (exports (car export-info))
           (rename-map (cdr export-info))
           (bindings (cdr record))
           ;; Apply export renames: change binding keys from internal to external names
           (renamed-bindings (map (lambda (b)
                                    (let ((rename (assq (car b) rename-map)))
                                      (if rename
                                          (cons (cdr rename) (cdr b))
                                          b)))
                                  bindings))
           ;; Filter by exported names (using external names)
           (exported (filter (lambda (b) (memq (car b) exports)) renamed-bindings)))
      (mc:apply-import-spec import-set exported))))

;; Extract defined identifiers from body forms
;; Handles: (define var ...) and (define (func args...) ...)
(define (mc:extract-module-defined-ids forms)
  (let loop ((forms forms) (ids '()))
    (if (null? forms)
        (reverse ids)
        (let ((form (car forms)))
          (loop (cdr forms)
                (if (and (pair? form) (eq? (car form) 'define))
                    (let ((pattern (cadr form)))
                      (cons (if (pair? pattern) (car pattern) pattern) ids))
                    ids))))))

;; Filter out define-syntax forms from body
(define (mc:filter-runtime-forms forms)
  (filter (lambda (f) (not (and (pair? f) (eq? (car f) 'define-syntax)))) forms))

;; Extract macro definitions from body and parse them
(define (mc:extract-macro-defs forms m-env s-env r-env)
  (let loop ((forms forms) (defs '()))
    (if (null? forms)
        (reverse defs)
        (let ((form (car forms)))
          (loop (cdr forms)
                (if (and (pair? form) (eq? (car form) 'define-syntax))
                    (let ((name (cadr form))
                          (transformer-spec (caddr form)))
                      (cons (cons name 
                                  (mc:make-macro-binding 
                                   (mc:parse-transformer transformer-spec (list m-env s-env r-env))))
                            defs))
                    defs))))))

;; Inject a binding into the interaction environment
(define (mc:inject-binding! name value)
  (set! mc:*temp-value* value)
  (eval `(define ,name mc:*temp-value*) (interaction-environment)))

;; Evaluate module body in an isolated lexical scope using a lambda wrapper.
;; Imports are quoted into defines; body forms are spliced in.
;; Internal definitions are name-mangled and injected globally to be visible to exported macros.
;; Defined values are collected and returned as an alist.

(define (mc:eval-module-body imported-bindings body-forms m-env s-env r-env mod-name)
  (let* ((macro-imports (filter (lambda (b) (mc:macro-binding? (cdr b))) imported-bindings))
         (runtime-imports (filter (lambda (b) (not (mc:macro-binding? (cdr b)))) imported-bindings))
         
         (inner-m-env (append (map (lambda (b) (cons (car b) (mc:unwrap-macro-binding (cdr b)))) macro-imports) m-env))

         ;; Pre-expand body forms to expose hidden definitions
         ;; We must NOT expand define-syntax forms here, as we need to extract them later
         (expanded-forms 
          (flatten-begins
           (map (lambda (form) 
                  (if (and (pair? form) (eq? (car form) 'define-syntax))
                      form
                      (mc:expand form inner-m-env s-env r-env)))
                body-forms)))

         (runtime-forms (mc:filter-runtime-forms expanded-forms))
         
         (defined-ids (mc:extract-module-defined-ids runtime-forms))

         (internal-mapping (map (lambda (id) (cons id (mc:mangle-name mod-name id))) defined-ids))

         (inner-r-env (append internal-mapping r-env))

         (macro-bindings (mc:extract-macro-defs expanded-forms inner-m-env s-env inner-r-env))
         
         (import-defs (map (lambda (b) `(define ,(car b) ',(cdr b))) 
                          runtime-imports))
         (wrapper `(lambda ()
                     ,@import-defs
                     ,@runtime-forms
                     (list ,@defined-ids))))
    (let* ((proc (eval wrapper (interaction-environment)))
           (vals (proc))
           (runtime-bindings (map cons defined-ids vals)))
      
      (for-each (lambda (p val)
                  (mc:inject-binding! (cdr p) val))
                internal-mapping vals)

      (append runtime-bindings macro-bindings))))



;; Expand define-module: define a module and register it in the module registry.
;; (define-module (name ...) decl ...)
;; Declarations can be export, import, or begin forms.
(define (mc:expand-define-module expr m-env s-env r-env)
  (let* ((mod-name (cadr expr))
         (decls (cddr expr))
         ;; Process declarations
         (export-specs (mc:scan-exports decls))
         (export-info (mc:process-export-specs export-specs))
         (exports (car export-info))
         (rename-map (cdr export-info))
         (imports (mc:scan-imports decls))
         (body-forms (mc:scan-begins decls))
         ;; Process imports
         (imported-bindings (apply append (map mc:process-import-set imports)))
         ;; Evaluate library body. Pass environment for macro parsing.
         (result-bindings (mc:eval-module-body imported-bindings body-forms m-env s-env r-env mod-name)))
    ;; Register the module
    (mc:register-module! mod-name exports rename-map (append result-bindings imported-bindings))
    ''defined))




;;=============================================================================
;; SECTION 4: Macro Transformers
;;=============================================================================
;;
;; Functions for creating and invoking macro transformers.
;; Transformers are procedures that take a syntax expression and return
;; an expanded syntax expression.

;; Create a syntax-rules transformer from a syntax-rules form.
;; form: the body of (syntax-rules ...), i.e., without the 'syntax-rules keyword
;; captured-context: the lexical context (m-env s-env r-env) where the macro was defined
;;
;; Handles optional custom ellipsis: (syntax-rules ellipsis (literals) ...)
;; If the specified ellipsis is in the literals list, generate a fresh one.
;;
;; Returns a transformer procedure that:
;; 1. Generates fresh suffixes for hygiene
;; 2. Creates a renamer that registers renamed identifiers
;; 3. Creates a literal=? predicate for matching literals
;; 4. Applies the syntax-rules pattern matching
(define (mc:make-syntax-rules-transformer form captured-context)
  (let* ((has-ellipsis? (symbol? (car form)))
         (ellipsis-in (if has-ellipsis? (car form) '...))
         (literals (if has-ellipsis? (cadr form) (car form)))
         (rules (if has-ellipsis? (cddr form) (cdr form)))
         (ellipsis (if (memq ellipsis-in literals) (gensym "ellipsis") ellipsis-in)))
    (lambda (expr)
      (let* ((suffix (mc:fresh-suffix))
             ;; Renamer: creates fresh names for introduced identifiers
             ;; and registers them in the rename environment
             (renamer (lambda (sym)
                        (if (eq? sym '...)
                            '...  ;; Don't rename the ellipsis
                            (let ((new-sym (mc:rename-symbol sym suffix)))
                              (mc:register-renamed! new-sym sym captured-context)
                              new-sym))))
             ;; Literal=?: checks if an input identifier matches a pattern literal
             ;; by comparing in the appropriate lexical context
             (literal=? (lambda (p-lit input)
                          (mc:core-form? input p-lit (cadr (or (mc:*current-context*) captured-context))))))
        (sr:apply-syntax-rules literals rules expr renamer ellipsis literal=?)))))

;; Invoke a macro transformer with the given expression and environment.
;; Sets up the current context parameter so the transformer can access
;; the lexical environment during expansion.
;; Handles both regular transformers and variable-transformers (for set! forms).
(define (mc:call-transformer transformer expr m-env s-env r-env)
  (parameterize ((mc:*current-context* (list m-env s-env r-env)))
    (syntax->datum (if (sc:variable-transformer? transformer)
                       ((sc:variable-transformer-procedure transformer) expr)
                       (transformer expr)))))

;; Parse a transformer specification and return a transformer procedure.
;; spec: the transformer specification (e.g., (syntax-rules ...) or (lambda ...))
;; context: the lexical context (m-env s-env r-env) for the transformer
;;
;; Supported transformer types:
;; 1. Macro expansion: if spec's head is a macro, expand it first
;; 2. syntax-rules: pattern-based macros
;; 3. lambda: procedural macros using syntax-case
;; 4. make-variable-transformer: for identifier macros that intercept set!
;; 5. identifier-syntax: shorthand for simple identifier macros
(define (mc:parse-transformer spec context)
  (let ((head (if (pair? spec) (car spec) #f)))
    (cond
      ;; If head is a macro, expand it first then parse the result
      ((and head (symbol? head) (mc:lookup-macro head (car context)))
       (mc:parse-transformer ((mc:lookup-macro head (car context)) spec) context))
      ;; syntax-rules: create a pattern-based transformer
      ((and (pair? spec) (mc:core-form? (car spec) 'syntax-rules (cadr context)))
       (mc:make-syntax-rules-transformer (cdr spec) context))
      ;; lambda: create a procedural transformer using syntax-case
      ;; The lambda receives a syntax object as input
      ((and (pair? spec) (mc:core-form? (car spec) 'lambda (cadr context)))
       (lambda (expr)
         (let ((input (sc:make-syntax-object expr context)))
           (let ((body (sc:prepare-eval-expr `((lambda ,(cadr spec) ,@(cddr spec)) ',input) '() '() '() context)))
             (syntax->datum (eval body (interaction-environment)))))))
      ;; make-variable-transformer: wrap a transformer to intercept set! forms
      ((and (pair? spec) (mc:core-form? (car spec) 'make-variable-transformer (cadr context)))
       (let ((proc (mc:parse-transformer (cadr spec) context)))
         (make-variable-transformer proc)))
      ;; identifier-syntax: shorthand for simple identifier macros
      ((and (pair? spec) (mc:core-form? (car spec) 'identifier-syntax (cadr context)))
       (let ((args (cdr spec)))
         (if (= (length args) 2)
             ;; Full form: (identifier-syntax (id1 template1) ((set! id2 var) template2))
             ;; Creates a variable-transformer that handles both read and write
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
             ;; Simple form: (identifier-syntax template)
             ;; Only handles read access
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
;;
;; Utility functions for handling lambda parameters and internal definitions.
;; These support the proper expansion of binding forms like lambda, let, letrec*.

;; Extract a flat list of parameter names from a lambda formals list.
;; Handles all lambda formal styles:
;; - (a b c)     => (a b c)           ; fixed arity
;; - (a b . rest) => (a b rest)       ; dotted pair (rest args)
;; - args        => (args)            ; single symbol (all args as list)
(define (mc:get-param-names params)
  (cond ((null? params) '())
        ((symbol? params) (list params))  ;; rest parameter
        ((pair? params) (cons (car params) (mc:get-param-names (cdr params))))
        (else '())))

;; Reconstruct a lambda formals structure with new parameter names.
;; Preserves the original structure (proper list, dotted pair, or symbol).
;; old-params: original formals structure
;; new-names: flat list of new names, same order as mc:get-param-names returns
(define (mc:reconstruct-params old-params new-names)
  (cond ((null? old-params) '())
        ((symbol? old-params) (car new-names))  ;; rest parameter stays as symbol
        ((pair? old-params) (cons (car new-names) (mc:reconstruct-params (cdr old-params) (cdr new-names))))
        (else '())))

;; Convert an internal define form to a binding pair (name value).
;; Handles both forms:
;; - (define (f x) body) => (f (lambda (x) body))
;; - (define x expr)     => (x expr)
(define (mc:internal-define->binding expr)
  (let ((pattern (cadr expr)))
    (if (pair? pattern)
        ;; Function definition: (define (f args...) body...)
        (list (car pattern) `(lambda ,(cdr pattern) ,@(cddr expr)))
        ;; Variable definition: (define x expr)
        (list pattern (caddr expr)))))

;; Extract internal define forms from the beginning of a body.
;; Returns a list of binding pairs ((name value) ...).
;; Also handles spliced begins: (begin (define...) (define...))
;; Stops when encountering a non-definition form.
(define (mc:extract-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ;; Internal define - convert to binding and continue
          ((and (pair? first) (mc:core-form? (car first) 'define shadowed-env))
           (cons (mc:internal-define->binding first) (mc:extract-internal-defines (cdr body) shadowed-env)))
          ;; Begin may contain spliced definitions - recurse into it
          ((and (pair? first) (mc:core-form? (car first) 'begin shadowed-env))
           (append (mc:extract-internal-defines (cdr first) shadowed-env) (mc:extract-internal-defines (cdr body) shadowed-env)))
          ;; Non-definition - stop extracting
          (else '())))))

;; Skip over internal define forms at the beginning of a body.
;; Returns the remaining body expressions after all definitions.
;; Also handles spliced begins.
(define (mc:skip-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ;; Skip internal define
          ((and (pair? first) (mc:core-form? (car first) 'define shadowed-env)) (mc:skip-internal-defines (cdr body) shadowed-env))
          ;; Skip defines inside begin
          ((and (pair? first) (mc:core-form? (car first) 'begin shadowed-env))
           (append (mc:skip-internal-defines (cdr first) shadowed-env) (mc:skip-internal-defines (cdr body) shadowed-env)))
          ;; Return remaining body
          (else body)))))

;;=============================================================================
;; SECTION 6: Expansion Handlers
;;=============================================================================
;;
;; Handlers for expanding specific Scheme core forms.
;; Each handler maintains hygiene by:
;; 1. Renaming bound identifiers to fresh names
;; 2. Updating the shadow environment to track what's locally bound
;; 3. Registering renamed identifiers for proper resolution
;; 4. Converting internal defines to letrec* bindings

;; Expand a lambda expression with hygiene.
;; Steps:
;; 1. Extract parameter names and generate fresh renamed versions
;; 2. Update environments: add to r-env for renaming, add to s-env for shadowing
;; 3. Reconstruct the parameter list with new names
;; 4. Extract internal defines from body and convert to letrec* if present
;; 5. Register renamed params in global rename env for hygiene tracking
;; 6. Recursively expand the body expressions
(define (mc:expand-lambda expr m-env s-env r-env)
  (let* ((params (cadr expr)) (body (cddr expr))
         ;; Get flat list of param names, handling dotted pairs
         (p-names (mc:get-param-names params))
         ;; Generate fresh names for each parameter
         (new-p-names (map (lambda (p) (mc:rename-symbol p (mc:fresh-suffix))) p-names))
         ;; Update rename env: original -> renamed
         (new-r-env (append (map cons p-names new-p-names) r-env))
         ;; Update shadow env: params shadow core forms
         (new-s-env (append p-names s-env))
         ;; Reconstruct formals with new names, preserving structure
         (new-params (mc:reconstruct-params params new-p-names))
         ;; Handle internal definitions (R6RS/R7RS)
         (i-defs (mc:extract-internal-defines body new-s-env))
         (r-body (mc:skip-internal-defines body new-s-env)))
    ;; Register all renamed parameters for hygiene resolution
    (for-each (lambda (p np) (mc:register-renamed! np p (list m-env new-s-env new-r-env))) p-names new-p-names)
    (if (null? i-defs)
        ;; No internal defines - just expand body
        `(lambda ,new-params ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) body)))
        ;; Has internal defines - wrap remaining body in letrec*
        `(lambda ,new-params ,(mc:expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env)))))

;; Expand a let expression with hygiene.
;; Handles both named let and regular let.
;; Named let: (let name ((var val) ...) body) transforms to letrec* with recursive binding
;; Regular let: expands init values first (in outer env), then expands body in extended env
(define (mc:expand-let expr m-env s-env r-env)
  (if (symbol? (cadr expr))
      ;; Named let: (let name ((var val) ...) body...)
      ;; Transform to: ((letrec* ((name (lambda (vars) body))) name) vals...)
      (let ((name (cadr expr)) (bindings (caddr expr)) (body (cdddr expr)))
        (let ((vars (map car bindings)) (vals (map cadr bindings)))
          (mc:expand `((letrec* ((,name (lambda ,vars ,@body))) ,name) ,@vals) m-env s-env r-env)))
      ;; Regular let: (let ((var val) ...) body...)
      (let* ((bindings (cadr expr)) (body (cddr expr))
             (vars (map car bindings)) (vals (map cadr bindings))
             ;; Expand init values in OUTER environment (let semantics)
             (expanded-vals (map (lambda (x) (mc:expand x m-env s-env r-env)) vals))
             ;; Generate fresh names for bound variables
             (new-vars (map (lambda (v) (mc:rename-symbol v (mc:fresh-suffix))) vars))
             ;; Build new environments for body
             (new-r-env (append (map cons vars new-vars) r-env))
             (new-s-env (append vars s-env))
             (new-bindings (map list new-vars expanded-vals))
             ;; Handle internal definitions in body
             (i-defs (mc:extract-internal-defines body new-s-env))
             (r-body (mc:skip-internal-defines body new-s-env)))
        ;; Register renamed variables for hygiene
        (for-each (lambda (v nv) (mc:register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
        (if (null? i-defs)
            ;; No internal defines - expand body directly
            `(let ,new-bindings ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) body)))
            ;; Has internal defines - wrap in letrec*
            `(let ,new-bindings ,(mc:expand `(letrec* ,i-defs ,@r-body) m-env new-s-env new-r-env))))))

;; Expand a letrec* expression with hygiene.
;; All bindings are visible in all init expressions (letrec* semantics).
;; Internal defines in body are merged with the explicit bindings.
;; Transforms to: (let ((v1 *undefined*) ...) (set! v1 init1) ... body...)
(define (mc:expand-letrec-star expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (body (cddr expr))
         ;; Extract and merge internal defines with explicit bindings
         (i-defs (mc:extract-internal-defines body s-env))
         (r-body (mc:skip-internal-defines body s-env))
         (all-bindings (append bindings i-defs))
         (vars (map car all-bindings)) (vals (map cadr all-bindings))
         ;; Generate fresh names - all visible in all init exprs
         (new-vars (map (lambda (v) (mc:rename-symbol v (mc:fresh-suffix))) vars))
         ;; Build environments - bindings visible in both inits and body
         (new-r-env (append (map cons vars new-vars) r-env))
         (new-s-env (append vars s-env))
         ;; Expand inits in NEW environment (letrec* semantics)
         (expanded-vals (map (lambda (x) (mc:expand x m-env new-s-env new-r-env)) vals))
         (new-bindings (map list new-vars expanded-vals)))
    ;; Register renamed variables for hygiene
    (for-each (lambda (v nv) (mc:register-renamed! nv v (list m-env new-s-env new-r-env))) vars new-vars)
    ;; Transform to: (let ((v *undefined*) ...) (set! v init) ... body)
    `(let ,(map (lambda (v) (list v ''*undefined*)) new-vars)
       ,@(map (lambda (v val) `(set! ,v ,val)) new-vars expanded-vals)
       ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env new-s-env new-r-env)) r-body)))))

;;=============================================================================
;; SECTION 7: Expansion Engine
;;=============================================================================
;;
;; The main expansion engine and handlers for macro-related forms.
;; This section contains:
;; - Macro definition forms (define-syntax, let-syntax, letrec-syntax)
;; - Derived form expanders (let*, letrec, cond, and, or, case)
;; - The handler lookup table and core form resolution

;; Expand define-syntax: register a global macro binding.
;; Parses the transformer spec and adds it to the global macro environment.
(define (mc:expand-define-syntax expr m-env s-env r-env)
  (mc:register-macro! (cadr expr) (mc:parse-transformer (caddr expr) (list m-env s-env r-env)))
  ''defined)

;; Expand let-syntax: create local macro bindings.
;; Transformers are parsed in the OUTER context.
;; The bound names shadow any existing core forms of the same name.
(define (mc:expand-let-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings)) (ctx (list m-env s-env r-env))
         ;; Parse each transformer in outer context (let-syntax semantics)
         (transformers (map (lambda (b) (mc:parse-transformer (cadr b) ctx)) bindings))
         ;; Add new macros to macro env
         (new-m-env (append (map cons names transformers) m-env))
         ;; Remove from shadow env so they're treated as macros, not shadowed
         (new-s-env (remove-from-list s-env names)))
    (mc:make-seq (map-improper (lambda (x) (mc:expand x new-m-env new-s-env r-env)) (cddr expr)))))

;; Expand letrec-syntax: create mutually recursive local macro bindings.
;; Uses a promise to allow macros to refer to each other.
;; Transformers see the new macro env being defined.
(define (mc:expand-letrec-syntax expr m-env s-env r-env)
  (let* ((bindings (cadr expr)) (names (map car bindings))
         ;; Create a promise that will be filled with the new env
         (env-promise (list 'promise #f))
         (ctx (list env-promise s-env r-env))
         ;; Parse transformers with the promise (they'll see final env)
         (transformers (map (lambda (b) (mc:parse-transformer (cadr b) ctx)) bindings))
         (new-m-env (append (map cons names transformers) m-env))
         (new-s-env (remove-from-list s-env names)))
    ;; Fill in the promise with the actual environment
    (set-car! (cdr env-promise) new-m-env)
    (mc:make-seq (map-improper (lambda (x) (mc:expand x new-m-env new-s-env r-env)) (cddr expr)))))

;; Expand let*-syntax: sequential local macro bindings.
;; Each binding sees the previous bindings.
;; Transforms to nested let-syntax forms.
(define (mc:expand-let*-syntax expr m-env s-env r-env)
  (let ((bindings (cadr expr)) (body (cddr expr)))
    (if (null? bindings)
        (mc:make-seq (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) body))
        ;; Expand to nested let-syntax
        (mc:expand `(let-syntax (,(car bindings)) (let*-syntax ,(cdr bindings) ,@body)) m-env s-env r-env))))

;; Expand let*: sequential binding form.
;; Empty bindings: (let* () body) => (let () body)
;; Single binding: (let* ((x v)) body) => (let ((x v)) body)
;; Multiple: (let* ((x v) rest...) body) => (let ((x v)) (let* (rest...) body))
(define (mc:expand-let* expr m-env s-env r-env)
  (let ((bits (cadr expr)) (body (cddr expr)))
    (cond ((null? bits) (mc:expand `(let () ,@body) m-env s-env r-env))
          ((null? (cdr bits)) (mc:expand `(let (,(car bits)) ,@body) m-env s-env r-env))
          (else (mc:expand `(let (,(car bits)) (let* ,(cdr bits) ,@body)) m-env s-env r-env)))))

;; Expand letrec: treats as letrec* for simplicity.
;; R6RS/R7RS allows this since letrec* is more general.
(define (mc:expand-letrec expr m-env s-env r-env)
  (mc:expand `(letrec* ,@(cdr expr)) m-env s-env r-env))

;; Expand set!: assignment with variable transformer support.
;; If the variable has a variable-transformer macro, invoke it.
;; Otherwise, look up the renamed variable and generate set!.
(define (mc:expand-set! expr m-env s-env r-env)
  (let* ((var (cadr expr))
         ;; Check if var is a macro (and not shadowed)
         (transformer (and (symbol? var) (not (memq var s-env)) (mc:lookup-macro var m-env))))
    (if (and transformer (sc:variable-transformer? transformer))
        ;; Variable transformer handles set! forms
        (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
        ;; Normal set! - look up renamed variable
        (let* ((pair (assq var r-env))
               (renamed (if pair (cdr pair) var)))
          `(set! ,renamed ,(mc:expand (caddr expr) m-env s-env r-env))))))

;; Expand if: directly expand all subexpressions.
(define (mc:expand-if expr m-env s-env r-env)
  `(if ,(mc:expand (cadr expr) m-env s-env r-env) ,(mc:expand (caddr expr) m-env s-env r-env)
       ,@(map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cdddr expr))))

;; Expand cond: transforms to nested if expressions.
;; Handles:
;; - (else body...) as final clause
;; - (test => proc) with arrow syntax
;; - (test body...) regular clauses
(define (mc:expand-cond expr m-env s-env r-env)
  (let ((clauses (cdr expr)))
    (if (null? clauses)
        '(begin)  ;; No clauses - return unspecified
        (let ((clause (car clauses)) (rest (cdr clauses)))
          (cond
            ;; else clause - just expand the body
            ((mc:core-form? (car clause) 'else s-env)
             (mc:expand (mc:make-seq (cdr clause)) m-env s-env r-env))
            ;; Arrow syntax: (test => proc) becomes (let ((t test)) (if t (proc t) ...))
            ((and (pair? (cdr clause)) (mc:core-form? (cadr clause) '=> s-env))
             (let ((tmp (mc:rename-symbol 'tmp (mc:fresh-suffix))))
               (mc:expand `(let ((,tmp ,(car clause)))
                          (if ,tmp
                              (,(caddr clause) ,tmp)
                              (cond ,@rest)))
                       m-env s-env r-env)))
            ;; Regular clause: (test body...) becomes (if test body (cond rest...))
            (else
             (mc:expand `(if ,(car clause)
                          ,(mc:make-seq (cdr clause))
                          (cond ,@rest))
                     m-env s-env r-env)))))))

;; Expand and: short-circuit logical and.
;; (and) => #t
;; (and x) => x
;; (and x y...) => (if x (and y...) #f)
(define (mc:expand-and expr m-env s-env r-env)
  (let ((args (cdr expr)))
    (cond
      ((null? args) #t)
      ((null? (cdr args)) (mc:expand (car args) m-env s-env r-env))
      (else
       (mc:expand `(if ,(car args) (and ,@(cdr args)) #f) m-env s-env r-env)))))

;; Expand or: short-circuit logical or.
;; (or) => #f
;; (or x) => x
;; (or x y...) => (let ((t x)) (if t t (or y...)))
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

;; Expand case: transforms to nested memv tests.
;; (case val ((datum...) body...) ...) =>
;; (let ((t val)) (if (memv t '(datum...)) body (if ...)))
(define (mc:expand-case expr m-env s-env r-env)
  (let ((val (cadr expr)) (clauses (cddr expr)))
    (let ((tmp (mc:rename-symbol 'tmp (mc:fresh-suffix))))
      (mc:expand `(let ((,tmp ,val))
                 ,(let recur ((clauses clauses))
                    (if (null? clauses)
                        '(begin)
                        (let ((clause (car clauses)) (rest (cdr clauses)))
                          (cond
                            ;; else clause
                            ((mc:core-form? (car clause) 'else s-env)
                             (mc:make-seq (cdr clause)))
                            ;; datum clause
                            (else
                             `(if (memv ,tmp ',(car clause))
                                  ,(mc:make-seq (cdr clause))
                                  ,(recur rest))))))))
              m-env s-env r-env))))

;; Expand define: handles both variable and function define forms.
;; (define (f args...) body...) => (define f (lambda (args...) body...))
;; (define x expr) => (define x expanded-expr)
(define (mc:expand-define expr m-env s-env r-env)
  (let ((head (cadr expr)))
    (if (pair? head)
        ;; Function definition - convert to lambda
        (mc:expand `(define ,(car head) (lambda ,(cdr head) ,@(cddr expr))) m-env s-env r-env)
        ;; Variable definition - expand the value
        `(define ,head ,@(flatten-begins (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cddr expr)))))))

;; Expand begin: flatten nested begins and expand all expressions.
(define (mc:expand-begin expr m-env s-env r-env)
  (mc:make-seq (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) (cdr expr))))

;; Expand quote: strip any rename suffixes from quoted data.
(define (mc:expand-quote expr m-env s-env r-env)
  `(quote ,(mc:strip-renames (cadr expr))))

;; Expand quasiquote: first expand the quasiquote structure, then expand the result.
(define (mc:expand-quasiquote-form expr m-env s-env r-env)
  (mc:expand (mc:expand-quasiquote (cadr expr)) m-env s-env r-env))



;; Helper: Check if a value is a macro binding wrapper
(define (mc:macro-binding? val)
  (and (pair? val) (eq? (car val) '**macro**)))

;; Helper: Create a macro binding wrapper
(define (mc:make-macro-binding trans)
  (cons '**macro** trans))

;; Helper: Unwrap a macro binding wrapper
(define (mc:unwrap-macro-binding val)
  (cdr val))

;; Expand import-module: import bindings from modules into the current environment.
;; (import-module import-set ...)
;; Import sets are processed and bindings are injected into the environment.
(define (mc:expand-import-module expr m-env s-env r-env)
  (let* ((import-sets (cdr expr))
         (all-bindings (apply append (map mc:process-import-set import-sets))))
    ;; Inject all bindings into the interaction environment
    (for-each (lambda (b) 
                (let ((name (car b)) (val (cdr b)))
                  (if (mc:macro-binding? val)
                      (mc:register-macro! name (mc:unwrap-macro-binding val))
                      (mc:inject-binding! name val))))
              all-bindings)
    ''imported))



;; Look up the handler function for a core form symbol.
;; Returns the handler procedure or #f if not a handled core form.
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
    ((quasiquote) mc:expand-quasiquote-form)
    ((define-module) mc:expand-define-module)
    ((import-module) mc:expand-import-module)
    (else #f)))

;; Resolve a symbol to its core form name, if it refers to one.
;; Returns the core form symbol if the identifier refers to an unshadowed core form,
;; or #f if it is shadowed by a local binding.
;; Handles renamed identifiers by checking in their original context.
(define (mc:resolve-core-form sym shadowed-env)
  (if (not (symbol? sym))
      #f
      (let ((resolved (mc:resolve-identifier sym)))
        (if (pair? resolved)
            ;; Renamed - check in original context
            (let ((context (car resolved)) (original (cdr resolved)))
              (mc:resolve-core-form original (mc:unwrap-env (cadr context))))
            ;; Not renamed - return the symbol if not shadowed
            (if (memq resolved (mc:unwrap-env shadowed-env))
                #f
                resolved)))))

;; Main expansion function.
;; Recursively expands a Scheme expression, handling macros and core forms.
;; 
;; Arguments (all optional):
;;   m-env: macro environment (defaults to '())
;;   s-env: shadow environment - identifiers that shadow core forms (defaults to '())
;;   r-env: rename environment - local identifier renamings (defaults to '())
;;
;; Expansion logic:
;; 1. Pair (application or special form):
;;    a. If head is a symbol:
;;       - If it's a macro and not shadowed, invoke transformer and re-expand
;;       - If it's a core form, use the appropriate handler
;;       - Otherwise, expand all subexpressions (function application)
;;    b. If head is not a symbol, expand all subexpressions
;; 2. Symbol (variable reference):
;;    - If it's a macro and not shadowed, invoke transformer and re-expand
;;    - Otherwise, resolve to its renamed form
;; 3. Other (literal): return as-is
(define (mc:expand expr . args)
  (let* ((m-env (if (null? args) '() (car args)))
         (s-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
         (r-env (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
    (cond
      ((pair? expr)
       (let ((head (car expr)))
         (if (symbol? head)
              ;; Try to find a macro transformer for the head
              (let ((transformer (and (not (memq head s-env)) (mc:lookup-macro head m-env))))
                (if transformer
                    ;; It's a macro - invoke and re-expand the result
                    (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
                    ;; Not a macro - check for core form handler
                    (let ((core-sym (mc:resolve-core-form head s-env)))
                      (let ((handler (and core-sym (mc:lookup-handler core-sym))))
                        (if handler
                            ;; Core form - use specific handler
                            (handler expr m-env s-env r-env)
                            ;; Not a core form - expand as function application
                            (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) expr))))))
              ;; Head is not a symbol - expand all parts
              (map-improper (lambda (x) (mc:expand x m-env s-env r-env)) expr))))
      ((symbol? expr)
       ;; Check if it's an identifier macro
       (let ((transformer (and (not (memq expr s-env)) (mc:lookup-macro expr m-env))))
         (if transformer
             ;; Identifier macro - invoke and re-expand
             (mc:expand (mc:call-transformer transformer expr m-env s-env r-env) m-env s-env r-env)
             ;; Not a macro - resolve to renamed form
             (mc:resolve-variable expr m-env s-env r-env))))
      ;; Literal values pass through unchanged
      (else expr))))

;;=============================================================================
;; SECTION 8: Cleanup & Entry Point
;;=============================================================================
;;
;; Functions for cleaning up expanded code and the public API.
;; - mc:strip-suffix: removes hygiene suffixes from symbol names
;; - mc:strip-renames: recursively strips suffixes from all symbols in an expression
;; - macroexpand-1: single-step macro expansion
;; - macroexpand: full recursive macro expansion

;; Strip the hygiene suffix from a symbol name.
;; Suffixes have the form ".N" where N is a number.
;; Recursively strips multiple suffixes (e.g., "x.1.2" -> "x").
(define (mc:strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            ;; Found a dot - check if suffix is all numeric
            (if (and (not (null? suffix)) (every? char-numeric? suffix))
                ;; Valid hygiene suffix - strip it and recurse
                (mc:strip-suffix (list->string (reverse (cdr chars))))
                ;; Not a hygiene suffix - return original
                str)
            (loop (cdr chars) (cons (car chars) suffix))))))

;; Strip all hygiene rename suffixes from an expression.
;; Recursively processes symbols, pairs, and vectors.
;; Used to produce clean output for quoted expressions and final results.
(define (mc:strip-renames expr)
  (let ((expr (syntax->datum expr)))
    (cond ((symbol? expr) (string->symbol (mc:strip-suffix (symbol->string expr))))
          ((pair? expr) (cons (mc:strip-renames (car expr)) (mc:strip-renames (cdr expr))))
          ((vector? expr) (list->vector (map mc:strip-renames (vector->list expr))))
          (else expr))))

;; Perform a single step of macro expansion.
;; If the expression is a macro call, apply the transformer once.
;; Does not recursively expand the result.
;; Useful for debugging macro expansion.
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

;; Main entry point for macro expansion.
;; Resets global state (rename counter and rename env) before expanding.
;; 
;; Arguments:
;;   expr: the expression to expand
;;   opt: optional flag 'strip to strip hygiene suffixes from the result
;;
;; Returns the fully expanded expression.
;; With 'strip option, all hygiene suffixes are removed for readability.
(define (macroexpand expr . opt)
  ;; Reset global state for a fresh expansion
  (set! mc:*rename-counter* 0)
  (set! mc:*rename-env* '())
  (let ((res (mc:expand expr '() '() '())))
    ;; Optionally strip rename suffixes for clean output
    (if (and (pair? opt) (eq? (car opt) 'strip)) (mc:strip-renames res) res)))
