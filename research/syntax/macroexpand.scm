;; macroexpand.scm
;; R6RS/R7RS-compatible macro expansion system for research purposes.
;;
;; This module provides the main macro expansion logic. The syntax-rules
;; pattern matching and template substitution is provided by syntax_rules.scm.
;;
;; Global state persists across calls for macro definitions and renaming.
;;
;; Supported Macros:
;;   - define-syntax, let-syntax, letrec-syntax, let*-syntax
;;   - syntax-rules with full R7RS features (ellipsis escape, custom ellipsis, literals)
;;
;; Supported Binding Forms:
;;   - lambda, let, let*, letrec, letrec*, named let
;;   - Internal defines (R6RS-style conversion to letrec*)
;;
;; Hygiene:
;;   - Alpha-renaming of bound identifiers
;;   - Captured context tracking for macro-introduced identifiers

;; Load syntax-rules implementation (pattern matching, binding, substitution)
(load "./syntax_rules.scm")
(load "./syntax_case.scm")

;; Global state for macro environment and renaming
(define *macro-env* '())
(define *rename-counter* 0)
(define *rename-env* '())

;;=========================================================================
;; STATE MANAGEMENT
;;=========================================================================

;; Generate a fresh unique suffix for alpha-renaming.
(define (fresh-suffix)
  (set! *rename-counter* (+ *rename-counter* 1))
  (number->string *rename-counter*))

;; Register a renamed identifier with its original name and capture context.
(define (register-renamed! alias original context)
  (set! *rename-env* (cons (list alias original context) *rename-env*)))

;; Register a global macro transformer.
(define (register-macro! name transformer)
  (set! *macro-env* (cons (cons name transformer) *macro-env*)))

;;=========================================================================
;; UTILITY FUNCTIONS
;;=========================================================================

;; Map over proper and improper lists.
;; For (a b . c), applies func to a, b, and c.
(define (map-improper func lst)
  (cond
    ((null? lst) '())
    ((pair? lst) (cons (func (car lst))
                        (map-improper func (cdr lst))))
    (else (func lst))))

;; Remove items from a list (uses memq for identity comparison).
(define (remove-from-list lst remove-items)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (loop (cdr lst)
              (if (memq (car lst) remove-items)
                  acc
                  (cons (car lst) acc))))))

;; Check if all elements satisfy predicate.
(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
            (every? pred (cdr lst)))))

;; Generate a list of integers [start, start+step, ...] of length count.
(define (iota count . start-and-step)
  (let ((start (if (null? start-and-step) 0 (car start-and-step)))
        (step (if (or (null? start-and-step)
                      (null? (cdr start-and-step)))
                  1
                  (cadr start-and-step))))
    (let loop ((n count) (val start) (acc '()))
      (if (<= n 0)
          (reverse acc)
          (loop (- n 1) (+ val step) (cons val acc))))))

;;=========================================================================
;; IDENTIFIER RESOLUTION
;;=========================================================================

;; Resolve an identifier through the rename environment.
;; Returns either the original symbol or a (context . original) pair
;; for macro-introduced identifiers.
(define (resolve-identifier id)
  (if (symbol? id)
      (let ((entry (assq id *rename-env*)))
        (if entry
            (let ((original (cadr entry))
                  (context (caddr entry)))
              (if context
                  (cons context original)
                  (resolve-identifier original)))
            id))
      id))

(define (rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

;; Construct a sequence, flattening nested begins and removing single-element wrappers.
;; Flatten a list of expressions by splicing in nested 'begin' forms.
(define (flatten-begins exprs)
  (let loop ((exprs exprs))
    (cond ((null? exprs) '())
          ((pair? exprs)
           (let ((first (car exprs)))
             (if (and (pair? first) (eq? (car first) 'begin))
                 (append (loop (cdr first)) (loop (cdr exprs)))
                 (cons first (loop (cdr exprs))))))
          (else (list exprs)))))

;; Construct a sequence, flattening nested begins and removing single-element wrappers.
(define (make-seq exprs)
  (let ((flat (flatten-begins exprs)))
    (cond ((null? flat) '(begin)) ;; Empty sequence
          ((null? (cdr flat)) (car flat)) ;; Single element
          (else `(begin ,@flat)))))

;; ... binding vars ...

;; ... define-syntax / compile-transformer ...

;; ... expand ...
;; Replace `(begin ...)` construction with (make-seq ...) in expand-let-syntax etc.

;; Note: since the file is monolithic, I will splice `make-seq` in UTILITY functions section
;; and update the `expand` function to use it.

;; Unwrap a promise-wrapped environment (used for letrec-syntax).
(define (unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise))
      (cadr env)
      env))

;; Look up a macro transformer by name in the given environment.
;; Handles resolution of renamed/captured identifiers.
(define (lookup-macro name env)
  (let ((resolved (resolve-identifier name)))
    (if (pair? resolved)
        ;; Macro-introduced identifier: look up in its captured context
        (let ((context (car resolved))
              (original (cdr resolved)))
          (lookup-macro original (unwrap-env (car context))))
        ;; Normal identifier: check local env, then global
        (let ((local-pair (assq resolved (unwrap-env env))))
          (if local-pair
              (cdr local-pair)
              (let ((global-pair (assq resolved *macro-env*)))
                (and global-pair (cdr global-pair))))))))

;; Check if sym refers to the core form 'name' (not shadowed by macros).
(define (core-form? sym name shadowed-env)
  (let ((resolved (resolve-identifier sym)))
    (if (pair? resolved)
        ;; Macro-introduced: check in its captured shadowed environment
        (let ((context (car resolved))
              (original (cdr resolved)))
          (core-form? original name (unwrap-env (cadr context))))
        ;; Normal: check if shadowed, then compare to name
        (and (not (memq resolved (unwrap-env shadowed-env)))
              (eq? resolved name)))))

;;=========================================================================
;; TRANSFORMER GENERATION
;;=========================================================================

;; Create a macro transformer from a syntax-rules form.
;; Captures the definition context for proper hygiene.
(define (make-syntax-rules-transformer form captured-context)
  (let* ((has-ellipsis? (symbol? (car form)))
          (ellipsis-in (if has-ellipsis? (car form) '...))
          (literals (if has-ellipsis? (cadr form) (car form)))
          (rules (if has-ellipsis? (cddr form) (cdr form)))
          ;; If ellipsis is in literals, use a gensym to avoid conflicts
          (ellipsis (if (memq ellipsis-in literals)
                        (gensym "ellipsis")
                        ellipsis-in)))
    (lambda (expr)
      (let* ((suffix (fresh-suffix))
              (renamer (lambda (sym)
                        (if (eq? sym '...)
                            '...
                            (let ((new-sym (rename-symbol sym suffix)))
                              (register-renamed! new-sym sym captured-context)
                              new-sym)))))
        (apply-syntax-rules literals rules expr renamer ellipsis)))))

;; Parse a transformer specification (must be syntax-rules or lambda).
(define (parse-transformer spec context)
  (cond
    ((and (pair? spec) (core-form? (car spec) 'syntax-rules (cadr context)))
     (make-syntax-rules-transformer (cdr spec) context))
    ((and (pair? spec) (core-form? (car spec) 'lambda (cadr context)))
     ;; Procedural macro
     (lambda (expr)
       (let ((input (make-syntax-object expr context)))
         ;; Transform the procedural macro body to eliminate host-level macros
         (let ((body (prepare-eval-expr `((lambda ,(cadr spec) ,@(cddr spec)) ',input) '() '() '())))
           (syntax->datum (eval body (interaction-environment)))))))
    (else (error "Only syntax-rules and lambda are supported for macros" spec))))

;;=========================================================================
;; BINDING FORM HELPERS
;;=========================================================================

;; Extract parameter names from a lambda formals list.
;; Handles: (a b c), (a b . c), and single symbol rest.
(define (get-param-names params)
  (cond
    ((null? params) '())
    ((symbol? params) (list params))
    ((pair? params) (cons (car params) (get-param-names (cdr params))))
    (else '())))

;; Reconstruct parameter structure with renamed symbols.
(define (reconstruct-params old-params new-names)
  (cond
    ((null? old-params) '())
    ((symbol? old-params) (car new-names))
    ((pair? old-params)
      (cons (car new-names)
            (reconstruct-params (cdr old-params) (cdr new-names))))
    (else '())))

;; Convert internal define to binding pair: (name init-expr).
;; Handles both (define name val) and (define (name args...) body...).
(define (internal-define->binding expr)
  (let ((pattern (cadr expr)))
    (if (pair? pattern)
        (list (car pattern)
              `(lambda ,(cdr pattern) ,@(cddr expr)))
        (list pattern (caddr expr)))))

;; Extract internal defines from body (including nested begin forms).
(define (extract-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (core-form? (car first) 'define shadowed-env))
            (cons (internal-define->binding first)
                  (extract-internal-defines (cdr body) shadowed-env)))
          ((and (pair? first) (core-form? (car first) 'begin shadowed-env))
            (append (extract-internal-defines (cdr first) shadowed-env)
                    (extract-internal-defines (cdr body) shadowed-env)))
          (else '())))))

;; Skip internal defines at the start of body, returning the rest.
(define (skip-internal-defines body shadowed-env)
  (if (null? body)
      '()
      (let ((first (car body)))
        (cond
          ((and (pair? first) (core-form? (car first) 'define shadowed-env))
            (skip-internal-defines (cdr body) shadowed-env))
          ((and (pair? first) (core-form? (car first) 'begin shadowed-env))
            (let ((rest-of-begin (skip-internal-defines (cdr first) shadowed-env)))
              (append rest-of-begin
                      (skip-internal-defines (cdr body) shadowed-env))))
          (else body)))))

;;=========================================================================
;; MAIN EXPANSION FUNCTION
;;=========================================================================

;; Recursively expand an expression.
;; Arguments:
;;   macro-env    - local macro environment (alist of name -> transformer)
;;   shadowed-env - list of symbols that shadow core forms
;;   rename-env   - local rename mappings (alist of original -> renamed)
(define (expand expr . args)
  (let* ((macro-env (if (null? args) '() (car args)))
          (shadowed-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
          (rename-env (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                          '()
                          (caddr args))))
    (cond
      ;; Compound expression
      ((pair? expr)
        (let ((head (car expr)))
          (if (symbol? head)
              (let ((transformer (and (not (memq head shadowed-env))
                                      (lookup-macro head macro-env))))
                (if transformer
                    ;; Macro invocation: expand the result
                    (expand (transformer expr) macro-env shadowed-env rename-env)
                    ;; Core form or procedure call
                    (cond
                      ;; define-syntax: register global macro
                      ((core-form? head 'define-syntax shadowed-env)
                      (let ((name (cadr expr))
                            (rule (caddr expr))
                            (context (list macro-env shadowed-env rename-env)))
                        (register-macro! name (parse-transformer rule context))
                        ''defined))

                      ;; let-syntax: local macros with parallel scope
                      ((core-form? head 'let-syntax shadowed-env)
                      (let* ((bindings (cadr expr))
                              (body (cddr expr))
                              (names (map car bindings))
                              (context (list macro-env shadowed-env rename-env))
                              (transformers (map (lambda (b)
                                                  (parse-transformer (cadr b) context))
                                                bindings))
                              (new-env (append (map cons names transformers) macro-env))
                              (new-shadowed (remove-from-list shadowed-env names)))
                        (make-seq (map-improper
                                    (lambda (x) (expand x new-env new-shadowed rename-env))
                                    body))))

                      ;; letrec-syntax: local macros with recursive scope
                      ((core-form? head 'letrec-syntax shadowed-env)
                      (let* ((bindings (cadr expr))
                              (body (cddr expr))
                              (names (map car bindings))
                              (env-promise (list 'promise #f))
                              (context (list env-promise shadowed-env rename-env))
                              (transformers (map (lambda (b)
                                                  (parse-transformer (cadr b) context))
                                                bindings))
                              (new-env (append (map cons names transformers) macro-env))
                              (new-shadowed (remove-from-list shadowed-env names)))
                        ;; Fulfill the promise with the completed environment
                        (set-car! (cdr env-promise) new-env)
                        (make-seq (map-improper
                                    (lambda (x) (expand x new-env new-shadowed rename-env))
                                    body))))

                      ;; let*-syntax: sequential local macros
                      ((core-form? head 'let*-syntax shadowed-env)
                      (let ((bindings (cadr expr))
                            (body (cddr expr)))
                        (if (null? bindings)
                            (make-seq (map-improper
                                        (lambda (x) (expand x macro-env shadowed-env rename-env))
                                        body))
                            (expand `(let-syntax (,(car bindings))
                                        (let*-syntax ,(cdr bindings) ,@body))
                                    macro-env shadowed-env rename-env))))

                      ;; lambda: alpha-rename parameters, handle internal defines
                      ((core-form? head 'lambda shadowed-env)
                      (let* ((params (cadr expr))
                              (body (cddr expr))
                              (param-names (get-param-names params))
                              (new-param-names (map (lambda (p)
                                                      (rename-symbol p (fresh-suffix)))
                                                    param-names))
                              (new-renames (append (map cons param-names new-param-names)
                                                  rename-env))
                              (new-shadowed (append param-names shadowed-env))
                              (new-params (reconstruct-params params new-param-names))
                              (internal-defs (extract-internal-defines body new-shadowed))
                              (rest-body (skip-internal-defines body new-shadowed)))
                        ;; Register renamed parameters
                        (for-each (lambda (p np)
                                    (register-renamed! np p #f))
                                  param-names new-param-names)
                        (if (null? internal-defs)
                            `(lambda ,new-params
                                ,@(flatten-begins
                                    (map-improper
                                      (lambda (x) (expand x macro-env new-shadowed new-renames))
                                      body)))
                            `(lambda ,new-params
                                ,(expand `(letrec* ,internal-defs ,@rest-body)
                                        macro-env new-shadowed new-renames)))))

                      ;; let: named let or regular let
                      ((core-form? head 'let shadowed-env)
                      (if (symbol? (cadr expr))
                          ;; Named let: transform to letrec*
                          (let ((name (cadr expr))
                                (bindings (caddr expr))
                                (body (cdddr expr)))
                            (let* ((vars (map car bindings))
                                    (vals (map cadr bindings)))
                              (expand `((letrec* ((,name (lambda ,vars ,@body)))
                                          ,name)
                                        ,@vals)
                                      macro-env shadowed-env rename-env)))
                          ;; Regular let
                          (let* ((bindings (cadr expr))
                                  (body (cddr expr))
                                  (vars (map car bindings))
                                  (vals (map cadr bindings))
                                  (expanded-vals (map (lambda (x)
                                                        (expand x macro-env shadowed-env rename-env))
                                                      vals))
                                  (new-vars (map (lambda (v)
                                                  (rename-symbol v (fresh-suffix)))
                                                vars))
                                  (new-renames (append (map cons vars new-vars) rename-env))
                                  (new-shadowed (append vars shadowed-env))
                                  (new-bindings (map list new-vars expanded-vals))
                                  (internal-defs (extract-internal-defines body new-shadowed))
                                  (rest-body (skip-internal-defines body new-shadowed)))
                            (for-each (lambda (v nv)
                                        (register-renamed! nv v #f))
                                      vars new-vars)
                            (if (null? internal-defs)
                                `(let ,new-bindings
                                    ,@(flatten-begins
                                        (map-improper
                                          (lambda (x) (expand x macro-env new-shadowed new-renames))
                                          body)))
                                `(let ,new-bindings
                                    ,(expand `(letrec* ,internal-defs ,@rest-body)
                                            macro-env new-shadowed new-renames))))))

                      ;; let*: transform to nested lets
                      ((core-form? head 'let* shadowed-env)
                      (let ((bindings (cadr expr))
                            (body (cddr expr)))
                        (cond
                          ((null? bindings)
                            (expand `(let () ,@body) macro-env shadowed-env rename-env))
                          ((null? (cdr bindings))
                            (expand `(let (,(car bindings)) ,@body)
                                    macro-env shadowed-env rename-env))
                          (else
                            (expand `(let (,(car bindings))
                                      (let* ,(cdr bindings) ,@body))
                                    macro-env shadowed-env rename-env)))))

                      ;; letrec*: sequential recursive bindings with internal defines
                      ((core-form? head 'letrec* shadowed-env)
                      (let* ((bindings (cadr expr))
                              (body (cddr expr))
                              (internal-defs (extract-internal-defines body shadowed-env))
                              (rest-body (skip-internal-defines body shadowed-env))
                              (all-bindings (append bindings internal-defs))
                              (vars (map car all-bindings))
                              (vals (map cadr all-bindings))
                              (new-vars (map (lambda (v)
                                              (rename-symbol v (fresh-suffix)))
                                            vars))
                              (new-renames (append (map cons vars new-vars) rename-env))
                              (new-shadowed (append vars shadowed-env))
                              (expanded-vals (map (lambda (x)
                                                    (expand x macro-env new-shadowed new-renames))
                                                  vals))
                              (new-bindings (map list new-vars expanded-vals)))
                        (for-each (lambda (v nv)
                                    (register-renamed! nv v #f))
                                  vars new-vars)
                        `(letrec* ,new-bindings
                            ,@(flatten-begins
                                (map-improper
                                  (lambda (x) (expand x macro-env new-shadowed new-renames))
                                  rest-body)))))

                      ;; letrec: treat as letrec*
                      ((core-form? head 'letrec shadowed-env)
                      (expand `(letrec* ,@(cdr expr)) macro-env shadowed-env rename-env))

                      ;; set!: resolve variable name
                      ((core-form? head 'set! shadowed-env)
                      (let* ((var (cadr expr))
                              (renamed-var (let ((pair (assq var rename-env)))
                                            (if pair (cdr pair) var))))
                        `(set! ,renamed-var
                                ,(expand (caddr expr) macro-env shadowed-env rename-env))))

                      ;; if: expand all subexpressions
                      ((core-form? head 'if shadowed-env)
                      `(if ,(expand (cadr expr) macro-env shadowed-env rename-env)
                            ,(expand (caddr expr) macro-env shadowed-env rename-env)
                            ,@(map-improper
                              (lambda (x) (expand x macro-env shadowed-env rename-env))
                              (cdddr expr))))

                      ;; define: expand body (not name)
                      ((core-form? head 'define shadowed-env)
                      `(define ,(cadr expr)
                          ,@(flatten-begins
                              (map-improper
                                (lambda (x) (expand x macro-env shadowed-env rename-env))
                                (cddr expr)))))

                      ;; begin: expand all subexpressions
                      ((core-form? head 'begin shadowed-env)
                      (make-seq (map-improper
                                  (lambda (x) (expand x macro-env shadowed-env rename-env))
                                  (cdr expr))))

                      ;; quote: return unchanged
                      ((core-form? head 'quote shadowed-env) expr)

                      ;; Procedure call: expand all parts
                      (else
                      (map-improper
                        (lambda (x) (expand x macro-env shadowed-env rename-env))
                        expr)))))
              ;; Non-symbol head: expand all parts
              (map-improper
              (lambda (x) (expand x macro-env shadowed-env rename-env))
              expr))))

      ;; Symbol: check for identifier macro, then resolve through rename environment
      ((symbol? expr)
        (let ((transformer (and (not (memq expr shadowed-env))
                                (lookup-macro expr macro-env))))
          (if transformer
              (expand (transformer expr) macro-env shadowed-env rename-env)
              (let ((pair (assq expr rename-env)))
                (if pair (cdr pair) expr)))))

      ;; Other atoms: return unchanged
      (else expr))))

;;=========================================================================
;; OUTPUT CLEANUP
;;=========================================================================

;; Strip numeric suffixes from symbols (e.g., x.1.2 -> x).
;; Recursively removes all .N suffixes where N is a non-empty digit sequence.
;; Preserves symbols like ... by requiring at least one digit after the dot.
(define (strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            (if (and (not (null? suffix))
                      (every? char-numeric? suffix))
                (strip-suffix (list->string (reverse (cdr chars))))
                str)
            (loop (cdr chars) (cons (car chars) suffix))))))

;; Strip rename suffixes from all symbols in an expression.
(define (strip-renames expr)
  (cond
    ((symbol? expr)
      (string->symbol (strip-suffix (symbol->string expr))))
    ((pair? expr)
      (cons (strip-renames (car expr))
            (strip-renames (cdr expr))))
    ((vector? expr)
      (list->vector (map strip-renames (vector->list expr))))
    (else expr)))

;;=========================================================================
;; ENTRY POINT
;;=========================================================================

;; Expand the expression.
;; Pass 'no-strip as optional argument to preserve rename suffixes.
(define macroexpand
  (lambda (expr . opt)
    (set! *rename-counter* 0)
    (set! *rename-env* '())
    (if (and (pair? opt) (eq? (car opt) 'strip))
        (strip-renames (expand expr '() '() '()))
        (expand expr '() '() '()))))
