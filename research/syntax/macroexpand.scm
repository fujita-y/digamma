;; macroexpand.scm
;; A simple macro expansion system for research purposes.
;; Supports define-syntax with syntax-rules.
;; R7RS-like Hygiene via renaming.

(load "syntax_rules.scm")

;; -- Identifier Renaming Logic --

;; Global counter for unique suffixes
(define *rename-counter* 0)
(define (fresh-suffix)
  (set! *rename-counter* (+ *rename-counter* 1))
  (number->string *rename-counter*))

;; A rename registry map: renamed-sym -> original-binding
;; original-binding can be:
;; - (core . name)
;; - (free . name)
;; - (bound . name)
(define *rename-env* '())

(define (register-renamed! alias original)
  (set! *rename-env* (cons (cons alias original) *rename-env*)))

(define (resolve-identifier id)
  (if (symbol? id)
      (let ((pair (assq id *rename-env*)))
        (if pair 
            (resolve-identifier (cdr pair)) ;; Recursive resolution
            id)) 
      id))

;; Rename a symbol unless it's a known core form in the "transformer environment"
;; For this simple impl, we just rename everything that isn't bound in the macro usage.
(define (make-renamer generic-rename-fn)
  (lambda (sym)
    (generic-rename-fn sym)))

(define (rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

;; -- Expansion Environment --

;; Global macro registry
(define *macro-env* '())

(define (register-macro! name transformer)
  (set! *macro-env* (cons (cons name transformer) *macro-env*)))

(define (lookup-macro nameenv . args)
  (let ((env (if (null? args) '() (car args))))
    ;; name might be renamed. Resolve it first.
    (let* ((resolved (resolve-identifier nameenv))
           (key (if (pair? resolved) (cdr resolved) resolved))) 
      ;; Check local env first
      (let ((local-pair (assq key env)))
        (if local-pair
            (cdr local-pair)
            (let ((global-pair (assq key *macro-env*)))
              (if global-pair (cdr global-pair) #f)))))))

;; Transformer constructor for syntax-rules
(define (make-syntax-rules-transformer form)
  (let* ((has-ellipsis? (symbol? (car form)))
         (ellipsis-in (if has-ellipsis? (car form) '...))
         (literals (if has-ellipsis? (cadr form) (car form)))
         (rules (if has-ellipsis? (cddr form) (cdr form)))
         ;; R7RS: If ellipsis is in literals, it is not an ellipsis.
         (ellipsis (if (memq ellipsis-in literals) (gensym "ellipsis") ellipsis-in)))
    (lambda (expr)
      ;; Create a fresh suffix for this expansion step
      (let* ((suffix (fresh-suffix))
             (renamer (lambda (sym)
                        (if (eq? sym '...)
                            '...
                            (let ((new-sym (rename-symbol sym suffix)))
                              (register-renamed! new-sym sym)
                              new-sym)))))
        (apply-syntax-rules literals rules expr renamer ellipsis)))))

;; Helper to check if a head matches a core form, respecting shadowing
(define (core-form? sym name shadowed-env)
  (if (memq sym shadowed-env)
      #f
      (let ((resolved (resolve-identifier sym)))
        (eq? resolved name))))

;; Helper to parse transformer specs
(define (parse-transformer spec shadowed-env)
  (if (and (pair? spec) (core-form? (car spec) 'syntax-rules shadowed-env))
      (make-syntax-rules-transformer (cdr spec))
      (error "Only syntax-rules is supported for local macros" spec)))

;; Helper for mapping over improper lists
(define (map-improper func lst)
  (cond
   ((null? lst) '())
   ((pair? lst)
    (cons (func (car lst))
          (map-improper func (cdr lst))))
   (else (func lst))))

;; Main expansion function
(define (expand expr . args)
  (let* ((macro-env (if (null? args) '() (car args)))
         (shadowed-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
         (rename-env (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
    (cond
     ((pair? expr)
      (let ((head (car expr)))
        (if (symbol? head)
            ;; Check if it's a macro (and not shadowed)
            ;; Note: head must be resolved regarding rename-env first?
            ;; If 'head' is 'x', and 'x' is renamed to 'x.1', we should check 'x.1'?
            ;; No, if x is renamed, it refers to a variable, not a macro (unless macro was passed as arg?)
            ;; But macros are usually top-level or let-syntax.
            ;; let-syntax names are NOT renamed in key lookup usually.
            ;; But if we rename 'let' vars, we should be consistent.
            ;; Simplified: Check simple head. 
            (let ((transformer (if (memq head shadowed-env) #f (lookup-macro head macro-env))))
              (if transformer
                  (expand (transformer expr) macro-env shadowed-env rename-env)
                  ;; Match core forms
                  (cond
                   ((core-form? head 'define-syntax shadowed-env)
                    (let ((name (cadr expr))
                          (rule (caddr expr)))
                      (if (and (pair? rule) (core-form? (car rule) 'syntax-rules shadowed-env))
                          (begin
                            (register-macro! name (make-syntax-rules-transformer (cdr rule)))
                            ''defined)
                          (error "Only syntax-rules is supported for define-syntax"))))
                   
                   ((core-form? head 'let-syntax shadowed-env)
                    (let* ((bindings (cadr expr))
                           (body (cddr expr))
                           (names (map car bindings))
                           (transformers (map (lambda (b) (parse-transformer (cadr b) shadowed-env)) bindings))
                           (new-env (append (map cons names transformers) macro-env))
                           (new-shadowed (remove-from-list shadowed-env names)))
                      `(begin ,@(map-improper (lambda (x) (expand x new-env new-shadowed rename-env)) body))))

                   ((core-form? head 'letrec-syntax shadowed-env)
                    (let* ((bindings (cadr expr))
                           (body (cddr expr))
                           (names (map car bindings))
                           (transformers (map (lambda (b) (parse-transformer (cadr b) shadowed-env)) bindings))
                           (new-env (append (map cons names transformers) macro-env))
                           (new-shadowed (remove-from-list shadowed-env names)))
                      `(begin ,@(map-improper (lambda (x) (expand x new-env new-shadowed rename-env)) body))))

                   ((core-form? head 'let*-syntax shadowed-env)
                    (let ((bindings (cadr expr))
                          (body (cddr expr)))
                      (if (null? bindings)
                          `(begin ,@(map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) body))
                          (expand `(let-syntax (,(car bindings))
                                     (let*-syntax ,(cdr bindings) ,@body))
                                  macro-env
                                  shadowed-env
                                  rename-env))))

                   ((core-form? head 'lambda shadowed-env)
                    (let* ((params (cadr expr))
                           (param-names (get-param-names params))
                           ;; Alpha-conversion: generate fresh names
                           (new-param-names (map (lambda (p) (rename-symbol p (fresh-suffix))) param-names))
                           ;; Map old -> new
                           (new-renames (append (map cons param-names new-param-names) rename-env))
                           (new-shadowed (append param-names shadowed-env))
                           ;; Reconstruct params with new names
                           (new-params (reconstruct-params params new-param-names)))
                      `(lambda ,new-params
                         ,@(map-improper (lambda (x) (expand x macro-env new-shadowed new-renames)) (cddr expr)))))
                   
                   ((core-form? head 'let shadowed-env)
                    (let* ((bindings (cadr expr))
                           (vars (map car bindings))
                           (vals (map cadr bindings))
                           ;; Vals expanded in CURRENT env
                           (expanded-vals (map (lambda (x) (expand x macro-env shadowed-env rename-env)) vals))
                           ;; Alpha-conversion
                           (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
                           (new-renames (append (map cons vars new-vars) rename-env))
                           (new-shadowed (append vars shadowed-env))
                           (new-bindings (map list new-vars expanded-vals)))
                      `(let ,new-bindings
                         ,@(map-improper (lambda (x) (expand x macro-env new-shadowed new-renames)) (cddr expr)))))
                   
                   ((core-form? head 'set! shadowed-env)
                    ;; target var might be renamed
                    (let* ((var (cadr expr))
                           (renamed-var (let ((pair (assq var rename-env))) (if pair (cdr pair) var))))
                      `(set! ,renamed-var ,(expand (caddr expr) macro-env shadowed-env rename-env))))
                    
                   ((core-form? head 'if shadowed-env)
                    `(if ,(expand (cadr expr) macro-env shadowed-env rename-env)
                         ,(expand (caddr expr) macro-env shadowed-env rename-env)
                         ,@(map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) (cdddr expr))))
                   
                   ((core-form? head 'define shadowed-env)
                    `(define ,(cadr expr)
                       ,@(map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) (cddr expr))))

                   ((core-form? head 'begin shadowed-env)
                    `(begin ,@(map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) (cdr expr))))
                   
                   ((core-form? head 'quote shadowed-env) expr)
                   
                   (else
                    (map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) expr)))))
            (map-improper (lambda (x) (expand x macro-env shadowed-env rename-env)) expr))))
     
     ((symbol? expr)
      ;; Rename variable if in rename-env
      (let ((pair (assq expr rename-env)))
        (if pair (cdr pair) expr)))
     
     (else expr))))

(define (reconstruct-params old-params new-names)
  (cond ((null? old-params) '())
        ((symbol? old-params) (car new-names))
        ((pair? old-params) (cons (car new-names) (reconstruct-params (cdr old-params) (cdr new-names))))
        (else '())))

;; Utilities
(define (get-param-names params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (get-param-names (cdr params))))
        (else '())))

(define (remove-from-list list remove-items)
  (if (null? list)
      '()
      (if (memq (car list) remove-items)
          (remove-from-list (cdr list) remove-items)
          (cons (car list) (remove-from-list (cdr list) remove-items)))))
