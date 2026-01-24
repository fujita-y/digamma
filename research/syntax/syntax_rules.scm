;; syntax_rules.scm
;; R7RS-compatible syntax-rules implementation for research.
;; Provides pattern matching against syntax-rules patterns and
;; template substitution with ellipsis expansion and hygiene support.



;; Helper: take
(define (take list n)
  (if (= n 0) '() (cons (car list) (take (cdr list) (- n 1)))))


;; Analyze pattern to compute nesting depth of each pattern variable.
;; Returns an alist mapping variable names to their ellipsis depth.
(define (analyze-pattern-vars pattern literals ellipsis depth)
  (cond
   ((and (symbol? pattern) 
         (not (memq pattern literals)) 
         (not (eq? pattern '_))
         (not (eq? pattern ellipsis)))
    (list (cons pattern depth)))
   
   ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
    (append (analyze-pattern-vars (car pattern) literals ellipsis (+ depth 1))
            (analyze-pattern-vars (cddr pattern) literals ellipsis depth)))
            
   ((pair? pattern)
    (append (analyze-pattern-vars (car pattern) literals ellipsis depth)
            (analyze-pattern-vars (cdr pattern) literals ellipsis depth)))
            
   ((vector? pattern)
    (analyze-pattern-vars (vector->list pattern) literals ellipsis depth))
    
   (else '())))

;; Main entry point: match input against rules and expand the matching template.
;; rename is a function that renames introduced identifiers for hygiene.
(define (apply-syntax-rules literals rules input rename ellipsis)
  ;; input is (macro-name arg ...)
  (let loop ((rules rules))
    (if (null? rules)
        (error "syntax-rules: no matching pattern" input)
        (let* ((rule (car rules))
               (pattern (car rule))
               (template (cadr rule)))
          (if (syntax-match? literals pattern input ellipsis)
              (let ((bindings (syntax-bind literals pattern input ellipsis))
                    (meta-env (analyze-pattern-vars pattern literals ellipsis 0)))
                (subst-template template bindings rename ellipsis meta-env 0 literals))
              (loop (cdr rules)))))))

;; Check if input matches the given pattern.
(define (syntax-match? literals pattern input ellipsis)
  (cond
    ;; Literal identifier: must match exactly
    ((and (symbol? pattern) (memq pattern literals))
     (and (symbol? input) (eq? pattern input)))
    
    ;; Pattern variable: matches anything (including underscore wildcard)
    ((symbol? pattern) #t)
    
    ;; Ellipsis pattern (P ... rest): match zero or more P, then match rest
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (and (list? input)
          (let loop ((xs input))
            (if (null? xs)
                (syntax-match? literals (cddr pattern) '() ellipsis)
                (if (syntax-match? literals (cddr pattern) xs ellipsis)
                    #t
                    (and (syntax-match? literals (car pattern) (car xs) ellipsis)
                         (loop (cdr xs))))))))
    
    ;; Pair: recursively match car and cdr
    ((pair? pattern)
     (and (pair? input)
          (syntax-match? literals (car pattern) (car input) ellipsis)
          (syntax-match? literals (cdr pattern) (cdr input) ellipsis)))
    
    ;; Vector: convert to list and match
    ((vector? pattern)
     (and (vector? input)
          (syntax-match? literals (vector->list pattern) (vector->list input) ellipsis)))
    
    ;; Constant datum: compare for equality
    (else (equal? pattern input))))

;; Extract bindings from input based on the pattern.
;; For ellipsis patterns, variables are bound to lists of matched values.
(define (syntax-bind literals pattern input ellipsis)
  (cond
    ((and (symbol? pattern) (memq pattern literals)) '())
    ((symbol? pattern)
     (if (eq? pattern '_) '() (list (cons pattern input))))
    
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (let loop ((xs input))
       (if (syntax-match? literals (cddr pattern) xs ellipsis)
           (append (syntax-bind literals (cddr pattern) xs ellipsis)
                   ;; For the ellipsis part, we need to bind variables to LISTS of values
                   (let ((p (car pattern)))
                     ;; Get vars in p
                     (let ((vars (pattern-vars p literals ellipsis)))
                        ;; For each var, collect list of matches
                        (map (lambda (v)
                               (cons v 
                                     (map (lambda (x)
                                            (let ((b (syntax-bind literals p x ellipsis)))
                                              (cdr (assq v b))))
                                          (take input (- (length input) (length xs))))))
                             vars))))
           (loop (cdr xs)))))
           
    ((pair? pattern)
     (append (syntax-bind literals (car pattern) (car input) ellipsis)
             (syntax-bind literals (cdr pattern) (cdr input) ellipsis)))
             
    ((vector? pattern)
     (syntax-bind literals (vector->list pattern) (vector->list input) ellipsis))
     
    (else '())))

;; Collect all pattern variable names from a pattern.
(define (pattern-vars pattern literals ellipsis)
  (cond
   ((and (symbol? pattern) 
         (not (memq pattern literals)) 
         (not (eq? pattern '_))
         (not (eq? pattern ellipsis)))
    (list pattern))
   ((pair? pattern)
    (append (pattern-vars (car pattern) literals ellipsis)
            (pattern-vars (cdr pattern) literals ellipsis)))
   (else '())))

;; Filter elements of lst satisfying pred (R5RS-compatible helper).
(define (my-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
        (else (my-filter pred (cdr lst)))))

;; Substitute bindings into the template, handling ellipsis expansion.
;; meta-env maps pattern variables to their ellipsis depth.
;; depth tracks current expansion level for proper ellipsis driver selection.
(define (subst-template template env rename ellipsis meta-env depth literals)
  (cond
   ;; Symbol: look up in bindings, or rename if unbound (hygiene)
   ((symbol? template)
    (let ((val (assq template env)))
      (if val 
          (cdr val) 
          (rename template))))
   
   ;; Escaped ellipsis: (... <tmpl>) disables ellipsis processing for <tmpl>
   ((and (pair? template) (eq? (car template) ellipsis))
    (if (and (pair? (cdr template)) (null? (cddr template)))
        (subst-template (cadr template) env rename (gensym "no-ellipsis") meta-env depth literals)
        (rename (car template))))

   ;; Ellipsis expansion: (sub-templ ... . rest)
   ;; Find driver variables (those at sufficient depth) and iterate over them.
   ((and (pair? template) (pair? (cdr template)) (eq? (cadr template) ellipsis))
    (let* ((p (car template))
           (all-vars (pattern-vars p '() ellipsis)) 
           ;; Driver variables: those bound at ellipsis depth >= current depth + 1
           (vars (my-filter (lambda (v) 
                              (let ((pair (assq v meta-env)))
                                (and pair (>= (cdr pair) (+ depth 1))))) 
                            all-vars))
           ;; Determine iteration count from first driver variable's binding length
           (len (if (null? vars) 
                    (let ((p-vars (pattern-vars p literals ellipsis)))
                       (if (null? p-vars)
                           0 
                           (error "too few ellipsis in pattern for template" template)))
                    (let ((binding (assq (car vars) env)))
                      (if binding (length (cdr binding)) 0))))
           ;; Build per-iteration environments by extracting i-th element from each driver
           (new-envs (map (lambda (i)
                            (map (lambda (v)
                                   (cons v (list-ref (cdr (assq v env)) i)))
                                 vars))
                          (iota len))))
      (append (map (lambda (sub-env) 
                     (subst-template p (append sub-env env) rename ellipsis meta-env (+ depth 1) literals)) 
                   new-envs)
              (subst-template (cddr template) env rename ellipsis meta-env depth literals))))
               
   ;; Pair: recursively substitute car and cdr
   ((pair? template)
    (cons (subst-template (car template) env rename ellipsis meta-env depth literals)
          (subst-template (cdr template) env rename ellipsis meta-env depth literals)))
          
   ;; Vector: convert to list, substitute, convert back
   ((vector? template)
    (list->vector (subst-template (vector->list template) env rename ellipsis meta-env depth literals)))
   
   ;; Constant datum: return as-is
   (else template)))
