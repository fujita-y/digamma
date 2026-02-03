;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Common utilities for the Digamma core system.
;; These functions provide a robust base for syntax expansion, optimization, and compilation.

;;=============================================================================
;; Predicates
;;=============================================================================

;; Check if expr is a proper list.
(define (proper-list? x)
  (let loop ((x x))
    (cond ((null? x) #t)
          ((pair? x) (loop (cdr x)))
          (else #f))))

;; Check if any element in the list satisfies the predicate.
(define (any pred lst)
  (and (not (null? lst))
       (or (pred (car lst)) (any pred (cdr lst)))))

(define any? any)

;; Check if all elements in the list satisfy the predicate.
(define (every pred lst)
  (or (null? lst) (and (pred (car lst)) (every pred (cdr lst)))))

(define every? every)

;;=============================================================================
;; List Transformation & Iteration
;;=============================================================================

;; Standard filter implementation.
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;; Standard fold-left implementation.
(define (fold proc seed lst)
  (if (null? lst) seed
      (fold proc (proc (car lst) seed) (cdr lst))))

;; Standard iota implementation: (iota 3) => (0 1 2)
(define (iota n)
  (let loop ((i 0))
    (if (= i n) '() (cons i (loop (+ i 1))))))

;; Take first n elements of a list.
(define (take lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Drop first n elements of a list.
(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

;; Map a function over a potentially improper list.
(define (map-improper func lst)
  (cond
    ((null? lst) '())
    ((pair? lst) (cons (func (car lst)) (map-improper func (cdr lst))))
    (else (func lst))))

;; Flatten nested 'begin' forms into a single list of expressions.
(define (flatten-begins exprs)
  (reverse
   (let loop ((input exprs) (acc '()))
     (cond ((null? input) acc)
           ((pair? input)
            (let ((head (car input)))
              (if (and (pair? head) (eq? (car head) 'begin))
                  ;; Found (begin ...), push its content onto input stream
                  (loop (append (cdr head) (cdr input)) acc)
                  ;; Found normal item, push to acc
                  (loop (cdr input) (cons head acc)))))
           ;; Improper list end or single non-list item
           (else (cons input acc))))))

;; Remove items from a list (uses memq for comparison).
(define (remove-from-list lst remove-items)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (loop (cdr lst) (if (memq (car lst) remove-items) acc (cons (car lst) acc))))))

;; Delete first occurrence of x in lst (uses equal? for comparison).
(define (delete x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

;;=============================================================================
;; Set Operations
;;=============================================================================

;; Union of multiple sets (lists treated as sets).
(define (set-union . sets)
  (fold (lambda (s acc)
          (fold (lambda (x a) (if (memq x a) a (cons x a))) acc s))
        '() sets))

;; Set difference: s1 minus s2.
(define (set-minus s1 s2)
  (filter (lambda (x) (not (memq x s2))) s1))

;; Pattern matcher for recursive let (loops)
(define (match-rec-pattern expr)
  (and (pair? expr)
       (eq? (car expr) 'let)
       (let ((bindings (cadr expr))
             (body (cddr expr)))
         (and (pair? bindings)
              (null? (cdr bindings))
              (let ((name (car (car bindings))))
                (and (symbol? name)
                     (pair? body)
                     (pair? (car body))
                     (eq? (car (car body)) 'set!)
                     (eq? (cadr (car body)) name)
                     (pair? (caddr (car body)))
                     (eq? (car (caddr (car body))) 'lambda)))))))
