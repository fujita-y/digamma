;; syntax_rules.scm
;; R7RS-style syntax-rules implementation (simplified research version)

;; This module provides the core pattern matching and template expansion logic.

(define (sr-matches? pattern input literals)
  (cond
    ((identifier? pattern)
     (if (memq pattern literals)
         (and (identifier? input) (eq? pattern input)) ;; Literal match
         #t)) ;; Variable match
    ((pair? pattern)
     (and (pair? input)
          (if (equal? (car pattern) '...)
              #f ;; ellipses should be handled by the list matcher
              (if (and (pair? (cdr pattern)) (equal? (cadr pattern) '...))
                  ;; (item ... . rest)
                  (let loop ((in input))
                     (if (or (null? in) (sr-matches? (cddr pattern) in literals))
                         (and (sr-matches? (cddr pattern) in literals)
                              (every? (lambda (x) (sr-matches? (car pattern) x literals))
                                      (take in (- (length in) (length (match-rest (cddr pattern) in))))))
                          (loop (cdr in))))
                   ;; simple pair
                   (and (sr-matches? (car pattern) (car input) literals)
                        (sr-matches? (cdr pattern) (cdr input) literals))))))
    ((vector? pattern)
     (and (vector? input)
          (sr-matches? (vector->list pattern) (vector->list input) literals)))
    (else (equal? pattern input))))

;; Helper: check if x is an identifier (symbol)
(define (identifier? x) (symbol? x))

;; Helper: every?
(define (every? pred list)
  (if (null? list) #t
      (and (pred (car list)) (every? pred (cdr list)))))

;; Helper: take
(define (take list n)
  (if (= n 0) '() (cons (car list) (take (cdr list) (- n 1)))))

;; Helper: match rest of the pattern against input to find where ellipsis ends
;; This is a simplification; a real implementation needs slightly robust backtracking or lookahead.
(define (match-rest pattern input)
    ;; returns the tail of input that matches pattern, or simplified:
    ;; assuming the `...` consumes greedily until the tail matches.
    ;; For this research code, we assume `...` is at the end of list or followed by something distinct.
    '()) ;; Placeholder for complex ellipsis handling

;; Main Transformer function
(define (syntax-rules-transformer literals rules)
  (lambda (expr kv-env)
     (let loop ((rules rules))
       (if (null? rules)
           (error "No matching syntax-rules pattern" expr)
           (let* ((rule (car rules))
                  (pattern (cdar rule)) ;; (pattern template) -> pattern usually includes the keyword at car
                  (template (cadr rule)))
             ;; Pattern usually starts with (_ ...) or (keyword ...)
             ;; The input expr also starts with the macro name.
             (if (sr-simplify-match? pattern expr literals)
                 (sr-expand template pattern expr literals kv-env)
                 (loop (cdr rules))))))))

;; A simplified matcher that returns binds or #f
;; pattern: (kw arg1 arg2 ...)
;; input:   (kw val1 val2 ...)
(define (sr-match pattern input literals binds)
  (cond
   ((identifier? pattern)
    (if (memq pattern literals)
        (if (and (identifier? input) (eq? input pattern)) binds #f)
        (cons (cons pattern input) binds)))
   ((pair? pattern)
    (cond
     ((and (pair? (cdr pattern)) (eq? (cadr pattern) '...))
      ;; Ellipsis: (p ... . rest)
      (let ((p (car pattern))
            (rest (cddr pattern)))
        (let loop ((in input) (acc '()))
           ;; Greedy match for p ...
           ;; This is naive. Correct way: lookahead to see if 'rest' matches.
           (if (null? in)
               (let ((b-rest (sr-match rest in literals binds)))
                 (if b-rest
                     (cons (cons (cons p '...) (reverse acc)) b-rest)
                     #f))
               (let ((b-head (sr-match p (car in) literals '())))
                 (if b-head
                     (loop (cdr in) (cons b-head acc))
                     ;; If p doesn't match head, maybe we are done with ... and text should match rest?
                     (sr-match rest in literals binds)))))))
     ((pair? input)
      (let ((b-car (sr-match (car pattern) (car input) literals binds)))
        (if b-car
            (sr-match (cdr pattern) (cdr input) literals b-car)
            #f)))
     (else #f)))
   ((vector? pattern)
    (if (vector? input)
        (sr-match (vector->list pattern) (vector->list input) literals binds)
        #f))
   (else (if (equal? pattern input) binds #f))))

;; Check match wrapper
(define (sr-simplify-match? pattern input literals)
  (not (eq? (sr-match pattern input literals '()) #f)))

;; Expansion
(define (sr-expand template pattern input literals kv-env)
  (let ((bindings (sr-match pattern input literals '())))
    (sr-expand-template template bindings kv-env)))

(define (sr-expand-template template bindings kv-env)
  (cond
   ((identifier? template)
    (let ((pair (assoc template bindings)))
      (if pair (cdr pair) template)))
   ((pair? template)
    (if (and (pair? (cdr template)) (eq? (cadr template) '...))
        (let* ((sub-templ (car template))
               ;; Find bindings relevant to sub-templ with '...'
               (ellipsis-vars (sr-find-ellipsis-vars sub-templ bindings)))
          ;; Construct list
          (sr-expand-ellipsis sub-templ ellipsis-vars (cddr template) bindings kv-env))
        (cons (sr-expand-template (car template) bindings kv-env)
              (sr-expand-template (cdr template) bindings kv-env))))
   ((vector? template)
    (list->vector (sr-expand-template (vector->list template) bindings kv-env)))
   (else template)))

(define (sr-find-ellipsis-vars template bindings)
  ;; Search bindings where key is present in template and value has form ((var . val) ...)
  ;; Simplified: Look for keys mapped to lists of bindings in bindings.
  ;; In our match structure, (p ... ...) stores list of sub-bindings in a specific way.
  '())

(define (sr-expand-ellipsis template vars rest bindings kv-env)
  ;; Expand template repeated N times
  '())

;; ---
;; RE-IMPLEMENTATION: The above was a sketch. Here is a cleaner, working recursive descent matcher.
;; ---

;; Analyze pattern to determine depth of each variable
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

;; Exported main function
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

(define (syntax-match? literals pattern input ellipsis)
  (cond
    ;; 1. Literal identifier match
    ((and (symbol? pattern) (memq pattern literals))
     (and (symbol? input) (eq? pattern input)))
    
    ;; 2. Pattern Variable (underscore is always a wildcard)
    ((symbol? pattern)
     (and (not (memq pattern literals))
          (or (eq? pattern '_) #t))) ;; binds anything
    
    ;; 3. Lists with ellipsis
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (and (list? input)
          (let loop ((xs input))
            (if (null? xs)
                (syntax-match? literals (cddr pattern) '() ellipsis)
                (if (syntax-match? literals (cddr pattern) xs ellipsis)
                    #t
                    (and (syntax-match? literals (car pattern) (car xs) ellipsis)
                         (loop (cdr xs))))))))
    
    ;; 4. Pairs
    ((pair? pattern)
     (and (pair? input)
          (syntax-match? literals (car pattern) (car input) ellipsis)
          (syntax-match? literals (cdr pattern) (cdr input) ellipsis)))
    
    ;; 5. Vectors
    ((vector? pattern)
     (and (vector? input)
          (syntax-match? literals (vector->list pattern) (vector->list input) ellipsis)))
    
    ;; 6. Constants
    (else (equal? pattern input))))

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

(define (my-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (my-filter pred (cdr lst))))
        (else (my-filter pred (cdr lst)))))

(define (subst-template template env rename ellipsis meta-env depth literals)
  (cond
   ((symbol? template)
    (let ((val (assq template env)))
      (if val 
          (cdr val) 
          (rename template))))
   
    ((and (pair? template) (eq? (car template) ellipsis))
     (if (and (pair? (cdr template)) (null? (cddr template)))
         ;; (... <tmpl>) -> substitution disabled for ellipsis identifier
         (subst-template (cadr template) env rename (gensym "no-ellipsis") meta-env depth literals)
         (rename (car template))))

   ((and (pair? template) (pair? (cdr template)) (eq? (cadr template) ellipsis))
    ;; (sub-templ ... . rest)
    (let* ((p (car template))
           (all-vars (pattern-vars p '() ellipsis)) 
           ;; Filter: var is driver iff (depth-in-pattern >= depth-in-template + 1)
           ;; Actually, we are entering ellipsis level 'depth + 1'.
           (vars (my-filter (lambda (v) 
                              (let ((pair (assq v meta-env)))
                                (and pair (>= (cdr pair) (+ depth 1))))) 
                            all-vars))
           
           (len (if (null? vars) 
                    (let ((p-vars (pattern-vars p literals ellipsis))) ;; Check if P has ANY vars
                       (if (null? p-vars)
                           0 
                           (error "too few ellipsis in pattern for template" template)))
                    (let ((binding (assq (car vars) env)))
                      (if binding (length (cdr binding)) 0))))
           
           (new-envs (map (lambda (i)
                            (map (lambda (v)
                                   (cons v (list-ref (cdr (assq v env)) i)))
                                 vars))
                          (iota len))))
      (append (map (lambda (sub-env) 
                     (subst-template p (append sub-env env) rename ellipsis meta-env (+ depth 1) literals)) 
                   new-envs)
              (subst-template (cddr template) env rename ellipsis meta-env depth literals))))
              
   ((pair? template)
    (cons (subst-template (car template) env rename ellipsis meta-env depth literals)
          (subst-template (cdr template) env rename ellipsis meta-env depth literals)))
          
   ((vector? template)
    (list->vector (subst-template (vector->list template) env rename ellipsis meta-env depth literals)))
    
   (else template)))
