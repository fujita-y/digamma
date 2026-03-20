;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; quasiquote.scm
;; Expand quasiquote (backquote) syntax into core forms: quote, list, cons, and append.
;; This implementation handles nested quasiquotation and splicing correctly according to R7RS.

(define (expand-quasiquote expr)
  
  ;; --- Identifier Predicates ---
  ;; These handle potential renaming/tagging by the macro expander (e.g., "unquote.123").

  (define (is-unquote? s)
    (and (symbol? s)
         (let ((name (symbol->string s)))
           (or (eq? s 'unquote)
               (and (> (string-length name) 8)
                    (string=? (substring name 0 8) "unquote."))))))

  (define (is-unquote-splicing? s)
    (and (symbol? s)
         (let ((name (symbol->string s)))
           (or (eq? s 'unquote-splicing)
               (and (> (string-length name) 17)
                    (string=? (substring name 0 17) "unquote-splicing."))))))

  (define (is-quasiquote? s)
    (and (symbol? s)
         (let ((name (symbol->string s)))
           (or (eq? s 'quasiquote)
               (and (> (string-length name) 11)
                    (string=? (substring name 0 11) "quasiquote."))))))

  ;; --- Analysis Helpers ---

  ;; Check if a list can be expanded using 'list' instead of 'cons'/'append'.
  ;; Returns #t if there's no unquote-splicing at the current expansion level.
  (define (no-splicing? x level)
    (if (= level 0)
        (let loop ((l x))
          (cond ((null? l) #t)
                ((not (pair? l)) #t)
                ((and (pair? (car l)) (is-unquote-splicing? (caar l))) #f)
                (else (loop (cdr l)))))
        #t))

  ;; Detects (a b ... . ,c) which should be (cons 'a (cons 'b c)) rather than (list 'a 'b c).
  (define (unquote-tail? x level)
    (if (= level 0)
        (let loop ((l x))
          (cond ((null? l) #f)
                ((not (pair? l)) #f)
                ((and (is-unquote? (car l))
                      (pair? (cdr l))
                      (null? (cddr l)))
                 #t)
                (else (loop (cdr l)))))
        #f))

  ;; --- Core Expansion Functions ---

  (define (expand-qq-list-splicing x level)
    (cond
     ((null? x) '())

     ;; Handle dotted tail: (a . b)
     ((not (pair? x))
      (expand-qq x level))

     ;; Handle ,@expr (splice) at level 0
     ((and (pair? (car x))
           (is-unquote-splicing? (caar x))
           (= level 0))
      (if (null? (cdr x))
          (cadar x)
          (list 'append (cadar x) (expand-qq-list-splicing (cdr x) level))))

     ;; Handle ,expr in dotted tail position: (a . ,b)
     ((and (is-unquote? (car x))
           (= level 0))
      (cadr x))

     ;; Normal element
     (else
      (list 'cons (expand-qq (car x) level)
            (expand-qq-list-splicing (cdr x) level)))))

  (define (expand-qq-list x level)
    (cond
     ((null? x) '())

     ;; Optimization: use 'list' if it's a proper list with no splicing or unquote-tail at level 0.
     ((and (proper-list? x)
           (no-splicing? x level)
           (not (unquote-tail? x level)))
      (cons 'list (map (lambda (e) (expand-qq e level)) x)))

     ;; Fallback to cons/append chain.
     (else
      (expand-qq-list-splicing x level))))

  (define (expand-qq x level)
    (cond
     ;; Handle ,expr (unquote)
     ((and (pair? x) (is-unquote? (car x)))
      (if (= level 0)
          (cadr x)
          (list 'list ''unquote (expand-qq (cadr x) (- level 1)))))

     ;; Handle `expr (nested quasiquote)
     ((and (pair? x) (is-quasiquote? (car x)))
      (list 'list ''quasiquote (expand-qq (cadr x) (+ level 1))))

     ;; Vectors
     ((vector? x)
      (let ((lst (vector->list x)))
        (if (null? lst)
            ''#()
            (list 'list->vector (expand-qq-list lst level)))))

     ;; Atoms and self-evaluating forms
     ((not (pair? x))
      (if (or (number? x) (boolean? x) (string? x) (null? x) (char? x))
          x
          (list 'quote x)))

     ;; Lists and pairs
     (else
      (expand-qq-list x level))))

  ;; Initial expansion call starting at level 0.
  (expand-qq expr 0))
