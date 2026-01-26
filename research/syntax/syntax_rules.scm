;; syntax_rules.scm
;; R7RS-compatible syntax-rules implementation for research.
;;
;; Provides pattern matching and template expansion for syntax-rules macros.
;; Optimized for performance by unifying matching and binding.

;;=============================================================================
;; SECTION 1: Utilities
;;=============================================================================

(define (sr-map-first-n lst n)
  (if (= n 0) '() (cons (car lst) (sr-map-first-n (cdr lst) (- n 1)))))

(define (sr-drop lst n)
  (if (= n 0) lst (sr-drop (cdr lst) (- n 1))))

(define (sr-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (sr-filter pred (cdr lst))))
        (else (sr-filter pred (cdr lst)))))

(define (sr-iota n)
  (let loop ((i 0))
    (if (= i n) '() (cons i (loop (+ i 1))))))

;;=============================================================================
;; SECTION 2: Pattern Analysis
;;=============================================================================

;; Collect pattern variables and their ellipsis depths.
;; Returns: alist ((var . depth) ...)
(define (sr-analyze-pattern p literals ellipsis depth)
  (cond
    ((symbol? p)
     (if (or (memq p literals) (eq? p '_) (eq? p ellipsis))
         '()
         (list (cons p depth))))
    ((pair? p)
     (if (and (pair? (cdr p)) (eq? (cadr p) ellipsis))
         (append (sr-analyze-pattern (car p) literals ellipsis (+ depth 1))
                 (sr-analyze-pattern (cddr p) literals ellipsis depth))
         (append (sr-analyze-pattern (car p) literals ellipsis depth)
                 (sr-analyze-pattern (cdr p) literals ellipsis depth))))
    ((vector? p)
     (sr-analyze-pattern (vector->list p) literals ellipsis depth))
    (else '())))

;;=============================================================================
;; SECTION 3: Unified Pattern Matching
;;=============================================================================

;; Matches input against pattern.
;; Returns: alist of bindings ((var . val) ...) on success, or #f on failure.
(define (sr-match-pattern literals pattern input ellipsis)
  (cond
    ;; Literal identifier: must match exactly
    ((and (symbol? pattern) (memq pattern literals))
     (and (symbol? input) (eq? pattern input) '()))

    ;; Wildcard: matches anything, no binding
    ((eq? pattern '_) (and '()))

    ;; Pattern variable: matches anything, binds to input
    ((symbol? pattern)
     (list (cons pattern input)))

    ;; Ellipsis pattern: (P ... . tail)
    ((and (pair? pattern) (pair? (cdr pattern)) (eq? (cadr pattern) ellipsis))
     (sr-match-ellipsis literals (car pattern) (cddr pattern) input ellipsis))

    ;; Pair: recursively match car and cdr
    ((pair? pattern)
     (and (pair? input)
          (let ((m-car (sr-match-pattern literals (car pattern) (car input) ellipsis)))
            (and m-car
                 (let ((m-cdr (sr-match-pattern literals (cdr pattern) (cdr input) ellipsis)))
                   (and m-cdr (append m-car m-cdr)))))))

    ;; Vector: match as list
    ((vector? pattern)
     (and (vector? input)
          (sr-match-pattern literals (vector->list pattern) (vector->list input) ellipsis)))

    ;; Constant datum
    (else (and (equal? pattern input) '()))))

;; Handles (P ... . tail) matching
(define (sr-match-ellipsis literals P tail input ellipsis)
  (letrec ((try-tail
            (lambda (xs head-bindings)
              (let ((m-tail (sr-match-pattern literals tail xs ellipsis)))
                (if m-tail
                    (let* ((p-vars (map car (sr-analyze-pattern P literals ellipsis 0)))
                           (transposed (map (lambda (v)
                                              (cons v (map (lambda (b) (cdr (assq v b))) (reverse head-bindings))))
                                            p-vars)))
                      (append transposed m-tail))
                    #f))))
           (loop (lambda (xs head-bindings)
                   (let ((m-p (if (pair? xs) (sr-match-pattern literals P (car xs) ellipsis) #f)))
                     (if m-p
                         (let ((res (loop (cdr xs) (cons m-p head-bindings))))
                           (if res res (try-tail xs head-bindings)))
                         (try-tail xs head-bindings))))))
    (loop input '())))

;;=============================================================================
;; SECTION 4: Template Expansion
;;=============================================================================

(define (sr-expand-template template bindings rename ellipsis meta-env depth)
  (cond
    ;; Symbol: lookup binding or rename
    ((symbol? template)
     (let ((b (assq template bindings)))
       (if b
           (cdr b)
           (rename template))))

    ;; Escaped Ellipsis: (... <tmpl>)
    ((and (pair? template) (eq? (car template) ellipsis))
     (if (and (pair? (cdr template)) (null? (cddr template)))
         (sr-expand-template (cadr template) bindings rename (gensym) meta-env depth)
         (rename (car template)))) ;; treat as symbol

    ;; Ellipsis Expansion: (tmpl ... . tail)
    ((and (pair? template) (pair? (cdr template)) (eq? (cadr template) ellipsis))
     (let* ((sub-tmpl (car template))
            (tail-tmpl (cddr template))
            ;; Find pattern vars in sub-tmpl that are drivers (depth >= current + 1)
            (p-vars (map car (sr-analyze-pattern sub-tmpl '() ellipsis 0)))
            (drivers (sr-filter (lambda (v)
                                  (let ((entry (assq v meta-env)))
                                    (and entry (>= (cdr entry) (+ depth 1)))))
                                p-vars))
            ;; Determine length
            (len (if (null? drivers)
                     0 
                     (let ((b (assq (car drivers) bindings)))
                       (if b (length (cdr b)) 0))))
            ;; Expand P for each i
            (expanded-list 
             (map (lambda (i)
                    (let ((iter-bindings
                           (map (lambda (v)
                                  (cons v (list-ref (cdr (assq v bindings)) i)))
                                drivers)))
                      (sr-expand-template sub-tmpl (append iter-bindings bindings) rename ellipsis meta-env (+ depth 1))))
                  (sr-iota len))))
       (append expanded-list 
               (sr-expand-template tail-tmpl bindings rename ellipsis meta-env depth))))

    ;; Pair
    ((pair? template)
     (cons (sr-expand-template (car template) bindings rename ellipsis meta-env depth)
           (sr-expand-template (cdr template) bindings rename ellipsis meta-env depth)))

    ;; Vector
    ((vector? template)
     (list->vector (sr-expand-template (vector->list template) bindings rename ellipsis meta-env depth)))

    ;; Constant
    (else template)))

;;=============================================================================
;; SECTION 5: Entry Point
;;=============================================================================

(define (apply-syntax-rules literals rules input rename ellipsis)
  (let loop ((rules rules))
    (if (null? rules)
        (error "syntax-rules: no matching pattern" input)
        (let* ((rule (car rules))
               (pattern (car rule))
               (template (cadr rule))
               (bindings (sr-match-pattern literals pattern input ellipsis)))
          (if bindings
              (let ((meta-env (sr-analyze-pattern pattern literals ellipsis 0)))
                (sr-expand-template template bindings rename ellipsis meta-env 0))
              (loop (cdr rules)))))))
