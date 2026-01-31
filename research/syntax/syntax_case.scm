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
;;
;; This implementation uses thunks (lambda wrappers) for delayed evaluation
;; of fenders and outputs to maintain proper lexical scoping within transformers.

(load "./syntax_common.scm")

;;=============================================================================
;; 1. Globals & State
;;=============================================================================
;;
;; Global state for managing syntax transformation process.
;; These are dynamically scoped during transformer execution.

;; Counter for generating unique temporary symbol names
(define sc:*syntax-temp-counter* 0)

;; Current pattern variable bindings: ((var . value) ...)
;; Populated during syntax-case matching and used during template expansion
(define sc:*current-syntax-bindings* '())

;; Current meta-environment tracking ellipsis depth: ((var . depth) ...)
;; Depth 0 means the variable was bound at outermost level
;; Depth N means the variable is inside N levels of ellipsis patterns
(define sc:*current-syntax-meta-env* '())

;;=============================================================================
;; 2. Utilities
;;=============================================================================
;;
;; Error reporting for syntax transformers.

;; Signal a syntax violation error.
;; who: optional transformer name (string or symbol)
;; message: description of the error
;; form: the offending form
;; subform: optional specific subform causing the error
(define (syntax-violation who message form . subform)
  (let ((who-str (if who (format "~a: " who) "")))
    (if (null? subform)
        (error (format "~asyntax-violation: ~a in ~s" who-str message form))
        (error (format "~asyntax-violation: ~a in ~s (subform: ~s)" who-str message form (car subform))))))

;;=============================================================================
;; 3. Syntax Objects & Identifiers
;;=============================================================================
;;
;; Syntax objects wrap datums with lexical context information.
;; This allows the macro system to track where identifiers were introduced
;; and compare them properly for hygiene.

;; A syntax object is a 3-element vector: #(**syntax-object** datum context)
;; - datum: the actual Scheme value (symbol, pair, vector, or constant)
;; - context: the lexical context (m-env s-env r-env) where it was created
;;
;; Only symbols, pairs, and vectors need wrapping; constants pass through as-is.
(define (sc:make-syntax-object datum context)
  (if (or (symbol? datum) (pair? datum) (vector? datum))
      (vector '**syntax-object** datum context)
      datum))

;; Check if an object is a syntax object.
(define (sc:syntax-object? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) '**syntax-object**)))

;; Extract the datum from a syntax object.
;; Returns the object itself if not a syntax object.
(define (sc:syntax-object-datum obj)
  (if (sc:syntax-object? obj) (vector-ref obj 1) obj))

;; Extract the lexical context from a syntax object.
;; Returns empty list if not a syntax object.
(define (sc:syntax-object-context obj)
  (if (sc:syntax-object? obj) (vector-ref obj 2) '()))

;; Convert a syntax object to a plain datum by stripping all context.
;; Recursively processes pairs and vectors.
(define (syntax->datum obj)
  (cond
    ((sc:syntax-object? obj) (syntax->datum (sc:syntax-object-datum obj)))
    ((pair? obj) (cons (syntax->datum (car obj)) (syntax->datum (cdr obj))))
    ((vector? obj) (list->vector (map syntax->datum (vector->list obj))))
    (else obj)))

;; Convert a datum to a syntax object, inheriting context from template-id.
;; This is the R6RS datum->syntax procedure.
(define (datum->syntax template-id datum)
  (sc:make-syntax-object datum (sc:syntax-object-context template-id)))

;; Create a variable transformer that can intercept set! forms.
;; The proc receives both reference and assignment forms.
(define (make-variable-transformer proc)
  (vector '**variable-transformer** proc))

;; Check if an object is a variable transformer.
(define (sc:variable-transformer? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) '**variable-transformer**)))

;; Extract the transformer procedure from a variable transformer.
(define (sc:variable-transformer-procedure obj)
  (vector-ref obj 1))

;; Convert a syntax object containing a list into a proper list of syntax objects.
;; Each element gets wrapped with the same context.
;; Used when iterating over ellipsis-matched sequences.
(define (sc:syntax->list obj)
  (let ((datum (sc:syntax-object-datum obj))
        (ctx (sc:syntax-object-context obj)))
    (cond
      ((null? datum) '())
      ((pair? datum)
       ;; Wrap each element with the same context
       (cons (sc:make-syntax-object (car datum) ctx)
             (sc:syntax->list (sc:make-syntax-object (cdr datum) ctx))))
      (else obj))))  ;; Not a list - return as-is

;; Check if an object is an identifier (symbol or wrapped symbol).
(define (sc:identifier? obj)
  (or (symbol? obj)
      (and (sc:syntax-object? obj) (symbol? (sc:syntax-object-datum obj)))))

;; Check if two identifiers are bound-identical.
;; They must have the same name AND the same lexical context.
;; This is used to check if two bindings refer to the same binding site.
(define (sc:bound-identifier=? id1 id2)
  (let ((d1 (sc:syntax-object-datum id1))
        (d2 (sc:syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2)
         ;; Must also have identical context for bound-identifier equality
         (equal? (sc:syntax-object-context id1) (sc:syntax-object-context id2)))))

;; Check if two identifiers are free-identical.
;; They must have the same name (ignoring context).
;; This is used to check if an identifier matches a literal in a pattern.
(define (sc:free-identifier=? id1 id2)
  (let ((d1 (sc:syntax-object-datum id1))
        (d2 (sc:syntax-object-datum id2)))
    (and (symbol? d1)
         (symbol? d2)
         (eq? d1 d2))))

;;=============================================================================
;; 4. Pattern Matching
;;=============================================================================
;;
;; Pattern matching for syntax-case. Matches input syntax against patterns
;; and returns bindings for pattern variables, or #f if no match.

;; Collect all pattern variable names from a pattern.
;; Excludes literals, underscore wildcard, and ellipsis.
;; Returns a flat list of symbols that are pattern variables.
(define (sc:collect-pattern-vars pattern literals ellipsis)
  (cond
    ;; A symbol that is not a literal, wildcard, or ellipsis is a pattern variable
    ((and (symbol? pattern)
          (not (member pattern literals))
          (not (eq? pattern '_))
          (not (eq? pattern ellipsis)))
     (list pattern))
    ;; Recursively collect from pairs
    ((pair? pattern)
     (append (sc:collect-pattern-vars (car pattern) literals ellipsis)
             (sc:collect-pattern-vars (cdr pattern) literals ellipsis)))
    (else '())))

;; Transpose a list of match results.
;; Given vars (v1 v2) and matches (((v1 . a1) (v2 . b1)) ((v1 . a2) (v2 . b2)))
;; Returns ((v1 a1 a2) (v2 b1 b2)) - each var maps to a list of matched values.
;; Used after matching ellipsis patterns to group values by variable.
(define (sc:transpose-matches vars matches)
  (map (lambda (v)
         (cons v (map (lambda (m) (cdr (assq v m))) matches)))
       vars))

;; Match an input syntax object against a pattern.
;; literals: list of literal identifiers that must match exactly
;; pattern: the pattern to match against
;; input: syntax object to match
;; ellipsis: the ellipsis symbol (usually '...)
;;
;; Returns: association list of (pattern-var . matched-value) or #f on failure.
;; For ellipsis patterns, the matched values are lists.
(define (sc:syntax-case-match literals pattern input ellipsis)
  (cond
    ;; Literal identifier: must match exactly using free-identifier=?
    ;; Returns empty bindings on success (literals don't bind)
    ((and (sc:identifier? pattern) (member (sc:syntax-object-datum pattern) literals))
     (and (sc:identifier? input)
          (sc:free-identifier=? pattern input)
          '()))

    ;; Wildcard (_): matches anything, binds nothing
    ((eq? pattern '_) '())

    ;; Pattern variable: matches any input and binds to it
    ((symbol? pattern) (list (cons pattern input)))

    ;; Ellipsis pattern: (P ... rest)
    ;; Matches zero or more occurrences of P, followed by rest
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (let* ((input-list (sc:syntax->list input))
            (p-vars (sc:collect-pattern-vars (car pattern) literals ellipsis)))
       (letrec (;; Try to match the rest of the pattern against remaining input
                (match-rest
                 (lambda (xs prefix-matches)
                   (let ((m-rest (sc:syntax-case-match literals (cddr pattern)
                                                     (if (sc:syntax-object? xs) xs (sc:make-syntax-object xs (sc:syntax-object-context input)))
                                                     ellipsis)))
                     (if m-rest
                         ;; Success - transpose prefix matches and combine with rest
                         (let ((transposed (sc:transpose-matches p-vars (reverse prefix-matches))))
                           (append transposed m-rest))
                         #f))))
                ;; Try to match more occurrences of P
                (loop (lambda (xs prefix-matches)
                        (let ((m-p (if (pair? xs) (sc:syntax-case-match literals (car pattern) (car xs) ellipsis) #f)))
                          (if m-p
                              ;; Matched one more P - try to continue
                              (let ((res (loop (cdr xs) (cons m-p prefix-matches))))
                                ;; If continuing fails, try matching rest here
                                (if res res (match-rest xs prefix-matches)))
                              ;; Couldn't match P - try matching rest
                              (match-rest xs prefix-matches))))))
         (loop input-list '()))))

    ;; Pair pattern: match car and cdr separately
    ((pair? pattern)
     (let ((input-datum (sc:syntax-object-datum input))
           (input-context (sc:syntax-object-context input)))
       (if (pair? input-datum)
           (let ((m1 (sc:syntax-case-match literals (car pattern) (sc:make-syntax-object (car input-datum) input-context) ellipsis))
                 (m2 (sc:syntax-case-match literals (cdr pattern) (sc:make-syntax-object (cdr input-datum) input-context) ellipsis)))
             (and m1 m2 (append m1 m2)))
           #f)))

    ;; Constant pattern: must equal exactly
    (else (if (equal? pattern (sc:syntax-object-datum input)) '() #f))))

;;=============================================================================
;; 5. Template Expansion
;;=============================================================================
;;
;; Expand syntax templates, replacing pattern variables with their bound values
;; and handling ellipsis replication.

;; Build a depth map for pattern variables.
;; Maps each pattern variable to its ellipsis depth (0 = not under ellipsis).
;; depth argument tracks current nesting level.
(define (sc:syntax-depth-map pattern literals ellipsis depth)
  (cond
    ;; Pattern variable - record its depth
    ((and (symbol? pattern) (not (member pattern literals))
          (not (eq? pattern '_)) (not (eq? pattern ellipsis)))
     (list (cons pattern depth)))
    ;; Ellipsis pattern - increase depth for the repeated part
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (append (sc:syntax-depth-map (car pattern) literals ellipsis (+ depth 1))
             (sc:syntax-depth-map (cddr pattern) literals ellipsis depth)))
    ;; Pair - recurse into both parts
    ((pair? pattern)
     (append (sc:syntax-depth-map (car pattern) literals ellipsis depth)
             (sc:syntax-depth-map (cdr pattern) literals ellipsis depth)))
    (else '())))

;; Expand a syntax template with the given bindings.
;; template: the template to expand
;; bindings: pattern variable bindings from matching
;; context: lexical context for introduced identifiers
;; meta-env: maps pattern variables to their ellipsis depth
;; depth: current ellipsis depth during expansion
;; ellipsis: the ellipsis symbol
;; literals: literal identifiers (not renamed)
;; suffix: unique suffix for hygiene renaming
(define (sc:expand-syntax template bindings context meta-env depth ellipsis literals suffix)
  (cond
    ;; Symbol in template
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if (and b (= (or (cdr (assq template meta-env)) 0) depth))
           ;; Pattern variable at correct depth - substitute its value
           (cdr b)
           (if (memq template literals)
               ;; Literal - keep as-is
               template
               ;; Introduced identifier - rename for hygiene
               (let ((new-sym (mc:rename-symbol template suffix)))
                 (mc:register-renamed! new-sym template context)
                 new-sym)))))
    ;; Pair in template
    ((pair? template)
     (if (and (pair? (cdr template)) (eq? (cadr template) ellipsis))
         ;; Template contains ellipsis: (P ... rest)
         ;; Replicate P for each value of the driving pattern variables
         (let* ((p (car template))
                (p-vars (sc:collect-pattern-vars p literals ellipsis))
                ;; Drivers are pattern variables with depth > current depth
                ;; These control how many times P is replicated
                (drivers (filter (lambda (v)
                                   (let ((pair (assq v meta-env)))
                                     (and pair (> (cdr pair) depth))))
                                 p-vars))
                ;; Get the length from the first driver's binding
                (len (if (null? drivers)
                         (syntax-violation 'syntax "ellipsis in template with no pattern variables" template)
                         (length (cdr (assq (car drivers) bindings)))))
                ;; Generate one expansion for each iteration
                (new-template-list
                 (map (lambda (i)
                        ;; Build iteration bindings: pick the i-th value for each driver
                        (let ((iter-bindings
                               (map (lambda (v)
                                      (cons v (list-ref (cdr (assq v bindings)) i)))
                                    drivers)))
                          (sc:expand-syntax p (append iter-bindings bindings) context meta-env (+ depth 1) ellipsis literals suffix)))
                      (iota len))))
           ;; Append the replicated list with the expanded rest
           (append new-template-list (sc:expand-syntax (cddr template) bindings context meta-env depth ellipsis literals suffix)))
         ;; No ellipsis - just expand car and cdr
         (cons (sc:expand-syntax (car template) bindings context meta-env depth ellipsis literals suffix)
               (sc:expand-syntax (cdr template) bindings context meta-env depth ellipsis literals suffix))))
    ;; Constant - return as-is
    (else template)))

;;=============================================================================
;; 6. Quasisyntax & Transformation
;;=============================================================================
;;
;; Quasisyntax provides quasi-quoting for syntax templates.
;; (quasisyntax template) is like (syntax template) but allows:
;; - (unsyntax expr): evaluate expr and splice result
;; - (unsyntax-splicing expr): evaluate expr and splice list elements
;;
;; The implementation extracts unsyntax forms, replaces them with temporaries,
;; and generates with-syntax bindings to evaluate them.

;; Extract unsyntax forms from a quasisyntax template.
;; Returns (template . bindings) where:
;; - template: the template with unsyntax replaced by temporary names
;; - bindings: list of (temp-name expr) pairs for with-syntax
;; depth tracks nesting of quasisyntax (unsyntax only works at depth 0)
(define (sc:extract-quasisyntax x depth literals)
  (cond
    ((pair? x)
     (cond
       ;; Nested quasisyntax - increase depth
       ((and (eq? (car x) 'quasisyntax) (pair? (cdr x)) (null? (cddr x)))
        (let ((res (sc:extract-quasisyntax (cadr x) (+ depth 1) literals)))
          (cons `(quasisyntax ,(car res)) (cdr res))))

       ;; unsyntax - extract at depth 0, otherwise decrease depth
       ((and (eq? (car x) 'unsyntax) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            ;; At depth 0: extract the expression
            (let ((tmp (string->symbol (string-append "unsyntax." (number->string (begin (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1)) sc:*syntax-temp-counter*))))))
              ;; Return temp as template, bind temp to the expression
              (cons (list '**splice-unsyntax** tmp) (list (list tmp (cadr x)))))
            ;; Nested: keep unsyntax, decrease depth
            (let ((res (sc:extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax ,(car res)) (cdr res)))))

       ;; unsyntax-splicing - like unsyntax but splices a list
       ((and (eq? (car x) 'unsyntax-splicing) (pair? (cdr x)) (null? (cddr x)))
        (if (= depth 0)
            ;; At depth 0: extract and mark for splicing with ellipsis
            (let ((tmp (string->symbol (string-append "unsyntax-splicing." (number->string (begin (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1)) sc:*syntax-temp-counter*))))))
              ;; Use (tmp ...) pattern to bind a list
              (cons (list '**splice-unsyntax-splicing** tmp) (list (list (list tmp '...) (cadr x)))))
            ;; Nested: keep unsyntax-splicing, decrease depth
            (let ((res (sc:extract-quasisyntax (cadr x) (- depth 1) literals)))
              (cons `(unsyntax-splicing ,(car res)) (cdr res)))))

       ;; Regular pair - recurse into car and cdr
       (else
        (let ((car-res (sc:extract-quasisyntax (car x) depth literals))
              (cdr-res (sc:extract-quasisyntax (cdr x) depth literals)))
          (let ((car-tmpl (car car-res))
                (cdr-tmpl (car cdr-res))
                (bindings (append (cdr car-res) (cdr cdr-res))))
            (cond
              ;; Handle **splice-unsyntax** marker - just cons the temp
              ((and (pair? car-tmpl) (eq? (car car-tmpl) '**splice-unsyntax**))
               (cons (cons (cadr car-tmpl) cdr-tmpl) bindings))
              ;; Handle **splice-unsyntax-splicing** marker - add ellipsis
              ((and (pair? car-tmpl) (eq? (car car-tmpl) '**splice-unsyntax-splicing**))
               (cons (append (list (cadr car-tmpl) '...) cdr-tmpl) bindings))
              (else
               (cons (cons car-tmpl cdr-tmpl) bindings))))))))

    ;; Vector - recursively extract from elements
    ((vector? x)
     (let ((res (sc:extract-quasisyntax (vector->list x) depth literals)))
       (cons (list->vector (car res)) (cdr res))))
    ;; Constant - pass through
    (else (cons x '()))))

;; Prepare an expression for evaluation in the transformer environment.
;; Transforms special forms (syntax, quasisyntax, syntax-case, with-syntax)
;; into runtime calls that use the current bindings.
;; This is used to compile the fender and output expressions of syntax-case.
;;
;; Key transformations:
;; - (syntax template) -> call to sc:expand-syntax at runtime
;; - (quasisyntax template) -> extract unsyntax, generate with-syntax
;; - (syntax-case input lits clauses) -> call to sc:expand-syntax-case
;; - (with-syntax bindings body) -> call to sc:expand-with-syntax
;; - Pattern variables at depth 0 -> quoted values
(define (sc:prepare-eval-expr expr literals meta-env bindings context)
  (let loop ((x expr))
    (cond
      ;; Symbol - substitute pattern variable values at depth 0
      ((symbol? x)
       (let ((b (assq x bindings)))
         (if (and b (= (or (cdr (assq x meta-env)) 0) 0))
             ;; Pattern variable at depth 0 - substitute its value
             (let ((val (cdr b)))
               (if (and (pair? val) (eq? (car val) 'quote))
                   val  ;; Already quoted
                   (list 'quote val)))  ;; Quote it
             x)))  ;; Not a pattern variable - keep as-is
      ((pair? x)
       (cond
         ;; (syntax template) - expand template at runtime
         ((eq? (car x) 'syntax)
          `(let ((suffix (mc:fresh-suffix)))
             (sc:expand-syntax ',(cadr x) sc:*current-syntax-bindings* ',context sc:*current-syntax-meta-env* 0 '... ',literals suffix)))
         ;; (quasisyntax template) - extract unsyntax, convert to with-syntax
         ((eq? (car x) 'quasisyntax)
          (let ((res (sc:extract-quasisyntax (cadr x) 0 literals)))
            (let ((new-tmpl (car res))
                  (bindings (cdr res)))
              (if (null? bindings)
                  ;; No unsyntax forms - just expand as syntax
                  `(let ((suffix (mc:fresh-suffix)))
                     (sc:expand-syntax ',new-tmpl sc:*current-syntax-bindings* ',context sc:*current-syntax-meta-env* 0 '... ',literals suffix))
                  ;; Has unsyntax forms - convert to with-syntax
                  (loop `(with-syntax ,bindings (syntax ,new-tmpl)))))))
         ;; (syntax-case input lits clauses...) - compile to runtime call
         ((eq? (car x) 'syntax-case)
          (let ((input (loop (cadr x)))
                (lits (caddr x))
                ;; Build a runtime list of (pattern fender-thunk output-thunk)
                (clauses-expr (cons 'list (map (lambda (c)
                                                 (let ((pat (car c))
                                                       ;; If no fender, use #t
                                                       (fender (if (null? (cddr c)) #t (cadr c)))
                                                       ;; Output is last element
                                                       (output (if (null? (cddr c)) (cadr c) (caddr c)))
                                                       (new-lits (append (caddr x) literals)))
                                                   ;; Wrap fender and output in thunks for deferred evaluation
                                                   `(list ',pat
                                                          (lambda () ,(sc:prepare-eval-expr fender new-lits meta-env bindings context))
                                                          (lambda () ,(sc:prepare-eval-expr output new-lits meta-env bindings context)))))
                                               (cdddr x)))))
            `(sc:expand-syntax-case ,input ',lits ,clauses-expr (interaction-environment))))
          ;; (with-syntax ((pat expr) ...) body...) - local pattern binding
          ((eq? (car x) 'with-syntax)
           (let ((b-specs (cadr x))
                 (body (cddr x)))
             `(sc:expand-with-syntax (list 'with-syntax (list ,@(map (lambda (s) `(list ',(car s) ,(loop (cadr s)))) b-specs)) (lambda () ,(loop (cons 'begin body)))) (interaction-environment))))
         ;; quote - pass through unchanged
         ((eq? (car x) 'quote) x)
         ;; Other pairs - recurse
         (else (cons (loop (car x)) (loop (cdr x))))))
      ;; Constants - pass through
      (else x))))

;;=============================================================================
;; 7. Macro Expansion Entry Points
;;=============================================================================
;;
;; Public API for syntax-case macro expansion.
;; These functions are called at runtime during transformer execution.

;; Generate a list of temporary identifiers for with-syntax bindings.
;; Takes a syntax object representing a list and returns fresh identifiers.
;; R6RS generate-temporaries procedure.
(define (generate-temporaries l)
  (let ((lst (sc:syntax->list l)))
    (map (lambda (x)
           (set! sc:*syntax-temp-counter* (+ sc:*syntax-temp-counter* 1))
           (sc:make-syntax-object (string->symbol (string-append "temp." (number->string sc:*syntax-temp-counter*))) '()))
         lst)))

;; Expand a with-syntax form.
;; with-syntax is like let for pattern variables:
;; (with-syntax ((pat expr) ...) body)
;; Matches each expr against its pat and binds pattern variables.
;; expr is the parsed with-syntax: (with-syntax bindings body-thunk)
(define (sc:expand-with-syntax expr env)
  (let ((bindings (cadr expr))
        (body-thunk (caddr expr)))
    ;; Convert to a single-clause syntax-case matching all patterns
    (sc:expand-syntax-case (sc:make-syntax-object (map cadr bindings) '())
                        '()
                        (list (list (map car bindings) (lambda () #t) body-thunk))
                        env)))

;; Main syntax-case expansion.
;; Matches input against each clause's pattern until one matches.
;; For matching clauses, evaluates the fender (guard) - if true, evaluates output.
;; Clauses are (pattern fender-thunk output-thunk).
;;
;; Process:
;; 1. Try each clause in order
;; 2. Match pattern against input
;; 3. If match succeeds, update bindings and meta-env
;; 4. Evaluate fender thunk - if truthy, evaluate output thunk
;; 5. If fender fails, restore state and try next clause
;; 6. If no clause matches, signal syntax-violation
(define (sc:expand-syntax-case input literals clauses env)
  (let ((ellipsis '...))
    (let loop ((clauses clauses))
      (if (null? clauses)
          (syntax-violation 'syntax-case "no matching pattern" input)
          (let* ((clause (car clauses))
                 (pattern (car clause))
                 (fender (cadr clause))
                 (output (caddr clause))
                 ;; Try to match the pattern
                 (m (sc:syntax-case-match literals pattern input ellipsis)))
            (if m
                ;; Pattern matched - extend bindings and try fender
                (let ((old-bindings sc:*current-syntax-bindings*)
                      (old-meta sc:*current-syntax-meta-env*)
                      (new-meta (sc:syntax-depth-map pattern literals ellipsis 0)))
                  ;; Add new bindings to current bindings
                  (set! sc:*current-syntax-bindings* (append m old-bindings))
                  (set! sc:*current-syntax-meta-env* (append new-meta old-meta))
                  ;; Evaluate fender
                  (let ((matched? (sc:eval-transformer-expr fender literals pattern sc:*current-syntax-bindings* env)))
                    (if matched?
                        ;; Fender passed - evaluate output
                        (let ((res (sc:eval-transformer-expr output literals pattern sc:*current-syntax-bindings* env)))
                          ;; Restore state and return result
                          (set! sc:*current-syntax-bindings* old-bindings)
                          (set! sc:*current-syntax-meta-env* old-meta)
                          res)
                        ;; Fender failed - try next clause
                        (begin
                          (set! sc:*current-syntax-bindings* old-bindings)
                          (set! sc:*current-syntax-meta-env* old-meta)
                          (loop (cdr clauses))))))
                ;; Pattern didn't match - try next clause
                (loop (cdr clauses))))))))

;; Evaluate a transformer expression (fender or output).
;; expr is either:
;; - A thunk (lambda) - just call it
;; - #t - return true (for default fenders)
;; - An expression - prepare and eval it
(define (sc:eval-transformer-expr expr literals pattern bindings env)
  (if (procedure? expr)
      ;; Thunk - just call it
      (expr)
      (if (eq? expr #t)
          ;; Default fender
          #t
          ;; Expression - prepare and evaluate
          (eval (sc:prepare-eval-expr expr literals sc:*current-syntax-meta-env* bindings '())
                (if (null? env) (interaction-environment) env)))))

