;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R7RS-compatible syntax-rules implementation.
;;
;; Provides pattern matching and template expansion for syntax-rules macros.
;; Optimized for performance by unifying matching and binding analysis.

;;=============================================================================
;; Pattern Analysis
;;=============================================================================

;; Collect pattern variables and their maximum ellipsis depths.
;; p: the pattern to analyze
;; literals: list of literal identifiers
;; ellipsis: the ellipsis symbol (usually '...)
;; depth: current ellipsis nesting depth
;; Returns: alist ((var . depth) ...)
(define (analyze-pattern p literals ellipsis depth)
  (cond
    ((symbol? p)
     (if (or (memq p literals) (eq? p '_) (eq? p ellipsis))
         '()
         (list (cons p depth))))
    ((pair? p)
     (if (and (pair? (cdr p)) (eq? (cadr p) ellipsis))
         ;; (P ... . tail) - P's variables have increased depth
         (append (analyze-pattern (car p) literals ellipsis (+ depth 1))
                 (analyze-pattern (cddr p) literals ellipsis depth))
         ;; (H . T) - Standard pair
         (append (analyze-pattern (car p) literals ellipsis depth)
                 (analyze-pattern (cdr p) literals ellipsis depth))))
    ((vector? p)
     (analyze-pattern (vector->list p) literals ellipsis depth))
    (else '())))

;;=============================================================================
;; Pattern Matching
;;=============================================================================

;; Matches input against a pattern containing ellipses.
;; This helper handles the greedy matching of the repeated part (P ...)
;; and ensures the tail matches correctly.
(define (match-ellipsis literals P tail input ellipsis literal=?)
  (letrec ((try-tail
            (lambda (xs head-bindings)
              (let ((m-tail (match-pattern literals tail xs ellipsis literal=?)))
                (if m-tail
                    (let* ((p-vars (map car (analyze-pattern P literals ellipsis 0)))
                           ;; Transpose bindings: group values for each pattern variable
                           (transposed (map (lambda (v)
                                              (cons v (map (lambda (b) (cdr (assq v b))) (reverse head-bindings))))
                                            p-vars)))
                      (append transposed m-tail))
                    #f))))
           (loop (lambda (xs head-bindings)
                   (let ((m-p (if (pair? xs) (match-pattern literals P (car xs) ellipsis literal=?) #f)))
                     (if m-p
                         ;; Greedily consume P, then try to match tail if subsequent matches fail
                         (let ((res (loop (cdr xs) (cons m-p head-bindings))))
                           (if res res (try-tail xs head-bindings)))
                         (try-tail xs head-bindings))))))
    (loop input '())))

;; Matches input against pattern.
;; literals: list of identifiers that must match exactly
;; pattern: the syntax-rules pattern
;; input: the expression to match
;; ellipsis: the ellipsis symbol
;; literal=?: predicate for comparing literals (hygiene-aware)
;; Returns: alist of bindings ((var . val) ...) on success, or #f on failure.
(define (match-pattern literals pattern input ellipsis literal=?)
  (cond
    ;; Literal identifier: must match exactly using provided predicate
    ((and (symbol? pattern) (memq pattern literals))
     (and (symbol? input) (literal=? pattern input) '()))

    ;; Wildcard: matches anything, binds no variables
    ((eq? pattern '_) (and '()))

    ;; Pattern variable: matches anything, binds to input
    ((symbol? pattern)
     (list (cons pattern input)))

    ;; Ellipsis pattern: (P ... . tail)
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (match-ellipsis literals (car pattern) (cddr pattern) input ellipsis literal=?))

    ;; Pair: recursively match car and cdr
    ((pair? pattern)
     (and (pair? input)
          (let ((m-car (match-pattern literals (car pattern) (car input) ellipsis literal=?)))
            (and m-car
                 (let ((m-cdr (match-pattern literals (cdr pattern) (cdr input) ellipsis literal=?)))
                   (and m-cdr (append m-car m-cdr)))))))

    ;; Vector: match as a list
    ((vector? pattern)
     (and (vector? input)
          (match-pattern literals (vector->list pattern) (vector->list input) ellipsis literal=?)))

    ;; Constant datum
    (else (and (equal? pattern input) '()))))

;;=============================================================================
;; Template Expansion
;;=============================================================================

;; Expands a template based on bindings from a successful match.
;; template: the template to expand
;; bindings: alist from match-pattern
;; rename: renamer function for introduced identifiers
;; ellipsis: the ellipsis symbol
;; meta-env: alist of pattern variables to their ellipsis depths
;; depth: current ellipsis nesting depth
(define (expand-template template bindings rename ellipsis meta-env depth)
  (cond
    ;; Symbol: lookup binding (if pattern variable) or rename (if introduced)
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if b
           (cdr b)
           (rename template))))

    ;; Escaped Ellipsis: (... <tmpl>) in R7RS
    ((and (pair? template) (eq? (car template) ellipsis))
     (if (and (pair? (cdr template)) (null? (cddr template)))
         ;; Expand the inner template with a fresh (unmatchable) ellipsis symbol
         (expand-template (cadr template) bindings rename (gensym) meta-env depth)
         (rename (car template))))

    ;; Ellipsis Expansion: (tmpl ... . tail)
    ((and (pair? template) (pair? (cdr template)) (eq? (cadr template) ellipsis))
     (let* ((sub-tmpl (car template))
            (tail-tmpl (cddr template))
            ;; Find pattern variables in sub-tmpl that are drivers for this ellipsis
            (p-vars (map car (analyze-pattern sub-tmpl '() ellipsis 0)))
            (drivers (filter (lambda (v)
                                  (let ((entry (assq v meta-env)))
                                    (and entry (>= (cdr entry) (+ depth 1)))))
                                p-vars))
            ;; Determine how many times to repeat based on driver lengths
            (len (if (null? drivers)
                     0 
                     (let ((b (assq (car drivers) bindings)))
                       (if b (length (cdr b)) 0))))
            ;; Expand sub-template for each iteration
            (expanded-list 
             (map (lambda (i)
                    (let ((iter-bindings
                           (map (lambda (v)
                                  (cons v (list-ref (cdr (assq v bindings)) i)))
                                drivers)))
                       (expand-template sub-tmpl (append iter-bindings bindings) rename ellipsis meta-env (+ depth 1))))
                  (iota len))))
       (append expanded-list 
               (expand-template tail-tmpl bindings rename ellipsis meta-env depth))))

    ;; Pair
    ((pair? template)
     (cons (expand-template (car template) bindings rename ellipsis meta-env depth)
           (expand-template (cdr template) bindings rename ellipsis meta-env depth)))

    ;; Vector
    ((vector? template)
     (list->vector (expand-template (vector->list template) bindings rename ellipsis meta-env depth)))

    ;; Constant
    (else template)))

;;=============================================================================
;; API Entry Point
;;=============================================================================

;; Primary entry point for syntax-rules application.
(define (apply-syntax-rules literals rules input rename ellipsis literal=?)
  (let loop ((rules rules))
    (if (null? rules)
        (error "syntax-rules: no matching pattern" input)
        (let* ((rule (car rules))
               (pattern (car rule))
               (template (cadr rule))
               (bindings (match-pattern literals pattern input ellipsis literal=?)))
          (if bindings
               ;; If pattern matches, analyze it for ellipsis depths and expand template
               (let ((meta-env (analyze-pattern pattern literals ellipsis 0)))
                 (expand-template template bindings rename ellipsis meta-env 0))
               (loop (cdr rules)))))))
