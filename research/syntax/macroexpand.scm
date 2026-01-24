;; macroexpand.scm
;; Macro expansion system for research purposes.
;; Supports define-syntax, let-syntax, letrec-syntax, and let*-syntax with syntax-rules.
;; Implements R7RS-style hygiene via alpha-renaming of introduced identifiers.

(load "syntax_rules.scm")

;; -- Identifier Renaming Logic --

;; Counter for generating unique suffixes during renaming.
(define *rename-counter* 0)
(define (fresh-suffix)
  (set! *rename-counter* (+ *rename-counter* 1))
  (number->string *rename-counter*))

;; Registry mapping renamed symbols to their original names and contexts.
;; Used to resolve identifiers back to their original form in their original scope.
(define *rename-env* '())

(define (register-renamed! alias original context)
  (set! *rename-env* (cons (list alias original context) *rename-env*)))

;; Resolve a renamed identifier.
;; Returns (context . original) if it has a captured context, otherwise just original.
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

;; Create a fresh symbol by appending a unique suffix.
(define (rename-symbol sym suffix)
  (string->symbol (string-append (symbol->string sym) "." suffix)))

;; -- Expansion Environment --

;; Global registry of top-level macro definitions.
(define *macro-env* '())

(define (register-macro! name transformer)
  (set! *macro-env* (cons (cons name transformer) *macro-env*)))

;; Look up a macro transformer by name.
;; Resolves renamed identifiers and checks local env before global.

(define (unwrap-env env)
  (if (and (pair? env) (eq? (car env) 'promise))
      (cadr env)
      env))

(define (lookup-macro name env)
  (let ((resolved (resolve-identifier name)))
    (if (pair? resolved)
        (let ((context (car resolved))
              (original (cdr resolved)))
          (lookup-macro original (unwrap-env (car context))))
        (let ((local-pair (assq resolved (unwrap-env env))))
          (if local-pair
              (cdr local-pair)
              (let ((global-pair (assq resolved *macro-env*)))
                (if global-pair (cdr global-pair) #f)))))))

;; Build a transformer procedure from a syntax-rules form.
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
                              new-sym)))))
        (apply-syntax-rules literals rules expr renamer ellipsis)))))

;; Check if sym refers to a core form, accounting for shadowing and renaming.
(define (core-form? sym name shadowed-env)
  (let ((resolved (resolve-identifier sym)))
    (if (pair? resolved)
        (let ((context (car resolved))
              (original (cdr resolved)))
          (core-form? original name (unwrap-env (cadr context))))
        (if (memq resolved (unwrap-env shadowed-env))
            #f
            (eq? resolved name)))))

;; Parse a transformer specification (must be a syntax-rules form).
(define (parse-transformer spec context)
  (if (and (pair? spec) (core-form? (car spec) 'syntax-rules (cadr context)))
      (make-syntax-rules-transformer (cdr spec) context)
      (error "Only syntax-rules is supported for local macros" spec)))

;; Map over a possibly improper list (handles dotted pairs).
(define (map-improper func lst)
  (cond
   ((null? lst) '())
   ((pair? lst)
    (cons (func (car lst))
          (map-improper func (cdr lst))))
   (else (func lst))))

;; Main expansion function.
;; macro-env: alist of local macro bindings
;; shadowed-env: list of names that shadow core forms
;; rename-env: alist for alpha-converted variable names
(define (expand expr . args)
  (let* ((macro-env (if (null? args) '() (car args)))
         (shadowed-env (if (or (null? args) (null? (cdr args))) '() (cadr args)))
         (rename-env (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
    (cond
     ((pair? expr)
      (let ((head (car expr)))
        (if (symbol? head)
            ;; Check if it's a macro (and not shadowed)
            ;; We look up the original name in the macro environment. 
            (let ((transformer (if (memq head shadowed-env) #f (lookup-macro head macro-env))))
              (if transformer
                  (expand (transformer expr) macro-env shadowed-env rename-env)
                  ;; Handle core forms
                  (cond
                   ;; define-syntax: register a global macro
                   ((core-form? head 'define-syntax shadowed-env)
                    (let ((name (cadr expr))
                          (rule (caddr expr))
                          (context (list macro-env shadowed-env rename-env)))
                      (if (and (pair? rule) (core-form? (car rule) 'syntax-rules shadowed-env))
                          (begin
                            (register-macro! name (make-syntax-rules-transformer (cdr rule) context))
                            ''defined)
                          (error "Only syntax-rules is supported for define-syntax"))))
                   
                   ;; let-syntax: local macros visible only in body
                   ((core-form? head 'let-syntax shadowed-env)
                    (let* ((bindings (cadr expr))
                           (body (cddr expr))
                           (names (map car bindings))
                           (context (list macro-env shadowed-env rename-env))
                           (transformers (map (lambda (b) (parse-transformer (cadr b) context)) bindings))
                           (new-env (append (map cons names transformers) macro-env))
                           (new-shadowed (remove-from-list shadowed-env names)))
                      `(begin ,@(map-improper (lambda (x) (expand x new-env new-shadowed rename-env)) body))))

                   ;; letrec-syntax: macros can refer to each other
                   ((core-form? head 'letrec-syntax shadowed-env)
                    (let* ((bindings (cadr expr))
                           (body (cddr expr))
                           (names (map car bindings))
                           ;; For letrec-syntax, we need a promise for the environment to handle mutual recursion.
                           (env-promise (list 'promise #f))
                           (context (list env-promise shadowed-env rename-env))
                           (transformers (map (lambda (b) (parse-transformer (cadr b) context)) bindings))
                           (new-env (append (map cons names transformers) macro-env))
                           (new-shadowed (remove-from-list shadowed-env names)))
                      (set-car! (cdr env-promise) new-env) ;; Fulfill the promise
                      `(begin ,@(map-improper (lambda (x) (expand x new-env new-shadowed rename-env)) body))))

                   ;; let*-syntax: sequential binding (each macro sees previous ones)
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

                   ;; lambda: alpha-convert parameters for hygiene
                   ((core-form? head 'lambda shadowed-env)
                    (let* ((params (cadr expr))
                           (param-names (get-param-names params))
                           (new-param-names (map (lambda (p) (rename-symbol p (fresh-suffix))) param-names))
                           (new-renames (append (map cons param-names new-param-names) rename-env))
                           (new-shadowed (append param-names shadowed-env))
                           (new-params (reconstruct-params params new-param-names)))
                      (for-each (lambda (p np) (register-renamed! np p #f)) param-names new-param-names)
                      `(lambda ,new-params
                         ,@(map-improper (lambda (x) (expand x macro-env new-shadowed new-renames)) (cddr expr)))))
                   
                   ;; let: handle both named and standard forms with alpha-conversion
                   ((core-form? head 'let shadowed-env)
                    (if (symbol? (cadr expr))
                        ;; Named let: transform to letrec + application
                        (let ((name (cadr expr))
                              (bindings (caddr expr))
                              (body (cdddr expr)))
                           (let* ((vars (map car bindings))
                                  (vals (map cadr bindings)))
                             (expand `((letrec* ((,name (lambda ,vars ,@body)))
                                         ,name)
                                       ,@vals)
                                     macro-env shadowed-env rename-env)))
                        ;; Standard let: expand values in outer scope, body in inner scope
                        (let* ((bindings (cadr expr))
                               (vars (map car bindings))
                               (vals (map cadr bindings))
                               (expanded-vals (map (lambda (x) (expand x macro-env shadowed-env rename-env)) vals))
                               (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
                               (new-renames (append (map cons vars new-vars) rename-env))
                               (new-shadowed (append vars shadowed-env))
                               (new-bindings (map list new-vars expanded-vals)))
                          (for-each (lambda (v nv) (register-renamed! nv v #f)) vars new-vars)
                          `(let ,new-bindings
                             ,@(map-improper (lambda (x) (expand x macro-env new-shadowed new-renames)) (cddr expr))))))
                   
                   ;; letrec*: sequential recursive binding (same as letrec for expansion)
                   ((core-form? head 'letrec* shadowed-env)
                    (let* ((bindings (cadr expr))
                           (vars (map car bindings))
                           (vals (map cadr bindings))
                           (new-vars (map (lambda (v) (rename-symbol v (fresh-suffix))) vars))
                           (new-renames (append (map cons vars new-vars) rename-env))
                           (new-shadowed (append vars shadowed-env))
                           (expanded-vals (map (lambda (x) (expand x macro-env new-shadowed new-renames)) vals))
                           (new-bindings (map list new-vars expanded-vals)))
                      (for-each (lambda (v nv) (register-renamed! nv v #f)) vars new-vars)
                      `(letrec* ,new-bindings
                         ,@(map-improper (lambda (x) (expand x macro-env new-shadowed new-renames)) (cddr expr)))))
                   ;; let*: sequential binding
                   ((core-form? head 'let* shadowed-env)
                    (let ((bindings (cadr expr))
                          (body (cddr expr)))
                      (cond ((null? bindings)
                             (expand `(let () ,@body) macro-env shadowed-env rename-env))
                            ((null? (cdr bindings))
                             (expand `(let (,(car bindings)) ,@body) macro-env shadowed-env rename-env))
                            (else
                             (expand `(let (,(car bindings))
                                        (let* ,(cdr bindings) ,@body))
                                     macro-env
                                     shadowed-env
                                     rename-env)))))

                   ;; letrec: recursive binding (same as letrec* for expansion)
                   ((core-form? head 'letrec shadowed-env)
                    (expand `(letrec* ,@(cdr expr)) macro-env shadowed-env rename-env))

                   ;; set!: resolve target variable through rename-env
                   ((core-form? head 'set! shadowed-env)
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
     
     ;; Symbol: look up in rename-env for alpha-converted variables
     ((symbol? expr)
      (let ((pair (assq expr rename-env)))
        (if pair (cdr pair) expr)))
     
     ;; Self-evaluating datum
     (else expr))))

;; Rebuild parameter list with renamed symbols (handles improper lists).
(define (reconstruct-params old-params new-names)
  (cond ((null? old-params) '())
        ((symbol? old-params) (car new-names))
        ((pair? old-params) (cons (car new-names) (reconstruct-params (cdr old-params) (cdr new-names))))
        (else '())))

;; Extract parameter names from a lambda formals list (handles rest args).
(define (get-param-names params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (get-param-names (cdr params))))
        (else '())))

;; Remove all items in remove-items from list.
(define (remove-from-list list remove-items)
  (if (null? list)
      '()
      (if (memq (car list) remove-items)
          (remove-from-list (cdr list) remove-items)
          (cons (car list) (remove-from-list (cdr list) remove-items)))))

;; -- Output Cleanup (Stripping Renames) --

(define (my-every? pred lst)
  (if (null? lst) #t
      (and (pred (car lst)) (my-every? pred (cdr lst)))))

(define (strip-suffix str)
  (let loop ((chars (reverse (string->list str))) (suffix '()))
    (if (null? chars)
        str
        (if (char=? (car chars) #\.)
            ;; Found dot from right
            (if (and (not (null? suffix)) (my-every? char-numeric? suffix))
                (strip-suffix (list->string (reverse (cdr chars))))
                (if (and (not (null? suffix)))
                    (let ((stripped (list->string (reverse (cdr chars)))))
                      ;; Recurse to strip more suffixes (e.g. .10.11 -> .10 -> "")
                      (strip-suffix stripped))
                    str))
            (loop (cdr chars) (cons (car chars) suffix))))))

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

(define (macroexpand expr)
  (strip-renames (expand expr '() '() '())))

