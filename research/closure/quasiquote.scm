;; quasiquote.scm
;; Expand quasiquote (backquote) syntax

;; Expand quasiquote to basic forms
;; `x => (quote x)
;; `,x => x
;; `(a ,b c) => (list 'a b 'c)
;; `(a ,@b c) => (cons 'a (append b (list 'c)))

;; Helper: check if expression is a proper list
(define (proper-list? expr)
  (cond
   ((null? expr) #t)
   ((not (pair? expr)) #f)
   (else (proper-list? (cdr expr)))))

(define (expand-quasiquote expr)
  (expand-qq expr 0))

;; Main expansion function with nesting level
(define (expand-qq expr level)
  (cond
   ;; Handle unquote
   ((and (pair? expr) (eq? (car expr) 'unquote))
    (if (= level 0)
        ;; At level 0, unquote evaluates
        (cadr expr)
        ;; Nested quasiquote, decrease level
        (list 'list ''unquote (expand-qq (cadr expr) (- level 1)))))
   
   ;; Handle quasiquote (nested)
   ((and (pair? expr) (eq? (car expr) 'quasiquote))
    ;; Increase nesting level
    (list 'list ''quasiquote (expand-qq (cadr expr) (+ level 1))))
   
   ;; Not a pair - quote it
   ((not (pair? expr))
    (list 'quote expr))
   
   ;; Handle list with potential unquote-splicing
   (else
    (expand-qq-list expr level))))

;; Expand a list, handling unquote-splicing
(define (expand-qq-list expr level)
  (cond
   ;; Empty list
   ((null? expr)
    ''())
   
   ;; Single element that is unquote-splicing at level 0
   ((and (pair? (car expr))
         (eq? (caar expr) 'unquote-splicing)
         (= level 0)
         (null? (cdr expr)))
    ;; ,@x when it's the only element
    (cadar expr))
   
   ;; First element is unquote-splicing at level 0
   ((and (pair? (car expr))
         (eq? (caar expr) 'unquote-splicing)
         (= level 0))
    ;; ,@x followed by more
    (list 'append (cadar expr) (expand-qq-list (cdr expr) level)))
   
   ;; Improper list (dotted pair)
   ((not (pair? (cdr expr)))
    (list 'cons (expand-qq (car expr) level)
          (expand-qq (cdr expr) level)))
   
   ;; Proper list - use cons
   (else
    (list 'cons (expand-qq (car expr) level)
          (expand-qq-list (cdr expr) level)))))

;; Alternative implementation using list construction
;; This version tries to optimize by using 'list' when possible
(define (expand-quasiquote-opt expr)
  (expand-qq-opt expr 0))

(define (expand-qq-opt expr level)
  (cond
   ;; Handle unquote
   ((and (pair? expr) (eq? (car expr) 'unquote))
    (if (= level 0)
        (cadr expr)
        (list 'list ''unquote (expand-qq-opt (cadr expr) (- level 1)))))
   
   ;; Handle quasiquote (nested)
   ((and (pair? expr) (eq? (car expr) 'quasiquote))
    (list 'list ''quasiquote (expand-qq-opt (cadr expr) (+ level 1))))
   
   ;; Not a pair - quote it
   ((not (pair? expr))
    (list 'quote expr))
   
   ;; List - try to build with list if no splicing
   (else
    (expand-qq-list-opt expr level))))

(define (expand-qq-list-opt expr level)
  (cond
   ;; Empty list
   ((null? expr)
    ''())
   
   ;; Check if it's a proper list first
   ((not (proper-list? expr))
    ;; Improper list, use cons-based expansion
    (expand-qq-list-with-splicing expr level))
   
   ;; Check if we can use 'list' (no unquote-splicing in proper list)
   ((no-splicing? expr level)
    (let ((elements (map (lambda (e) (expand-qq-opt e level)) expr)))
      (cons 'list elements)))
   
   ;; Has splicing, use cons/append
   (else
    (expand-qq-list-with-splicing expr level))))

(define (expand-qq-list-with-splicing expr level)
  (cond
   ((null? expr)
    ''())
   
   ;; First element is unquote-splicing at level 0
   ((and (pair? (car expr))
         (eq? (caar expr) 'unquote-splicing)
         (= level 0))
    (if (null? (cdr expr))
        (cadar expr)
        (list 'append (cadar expr) (expand-qq-list-with-splicing (cdr expr) level))))
   
   ;; Improper list
   ((not (pair? (cdr expr)))
    (list 'cons (expand-qq-opt (car expr) level)
          (expand-qq-opt (cdr expr) level)))
   
   ;; Regular element
   (else
    (list 'cons (expand-qq-opt (car expr) level)
          (expand-qq-list-with-splicing (cdr expr) level)))))

;; Check if list has no unquote-splicing at current level
(define (no-splicing? expr level)
  (cond
   ((not (pair? expr)) #t)
   ;; Check if it's an improper list (has dotted tail)
   ((and (pair? expr) (not (or (null? (cdr expr)) (pair? (cdr expr)))))
    ;; Improper list - check car only, cdr is atomic
    (if (and (pair? (car expr))
             (eq? (caar expr) 'unquote-splicing)
             (= level 0))
        #f
        (no-splicing? (car expr) level)))
   ((and (pair? (car expr))
         (eq? (caar expr) 'unquote-splicing)
         (= level 0))
    #f)
   ((and (pair? (car expr))
         (eq? (caar expr) 'quasiquote))
    (no-splicing? (cdr expr) level))
   (else
    (and (no-splicing? (car expr) level)
         (no-splicing? (cdr expr) level)))))

;; Main entry point (use optimized version)
(define (qq-expand expr)
  (expand-quasiquote-opt expr))
