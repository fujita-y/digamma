;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Common utilities for the Digamma core system.
;; These functions provide a robust base for syntax expansion, optimization, and compilation.

(cond-expand
  (gauche
   (use srfi-9)
   (define (make-eq-hashtable) (make-hash-table 'eq?))
   (define (make-eqv-hashtable) (make-hash-table 'eqv?))
   (define (make-equal-hashtable) (make-hash-table 'equal?))
   (define (make-hashtable hash-fn equiv-fn)
     (cond ((eq? equiv-fn equal?) (make-equal-hashtable))
           ((eq? equiv-fn eqv?) (make-eqv-hashtable))
           ((eq? equiv-fn eq?) (make-eq-hashtable))
           (else (make-hash-table equiv-fn))))
   (define hashtable-clear! hash-table-clear!)
   (define hashtable-contains? hash-table-exists?)
   (define hashtable-delete! hash-table-delete!)
   (define hashtable-ref hash-table-get)
   (define hashtable-set! hash-table-put!)
   (define hashtable->alist hash-table->alist)
   (define (equal-hash obj) (hash obj)))
  (ypsilon
   (import (srfi 9))
   (define (hashtable->alist ht)
     (let-values (((keys vals) (hashtable-entries ht)))
       (map cons (vector->list keys) (vector->list vals)))))
  (else
   ;; nanos
   ))

;;=============================================================================
;; Globals & State
;;=============================================================================

;; Counter for generating unique temporary symbol names
(define *syntax-temp-counter* 0)

;;=============================================================================
;; Predicates
;;=============================================================================

;; Returns #t if x is a proper list, #f otherwise.
(define (proper-list? x)
  (let loop ((x x))
    (cond ((null? x) #t)
          ((pair? x) (loop (cdr x)))
          (else #f))))

;; Returns #t if any element of lst satisfies pred.
(define (any pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((pred (car lst)) #t)
          (else (loop (cdr lst))))))

(define any? any)

;; Returns #t if every element of lst satisfies pred.
(define (every pred lst)
  (let loop ((lst lst))
    (cond ((null? lst) #t)
          ((pred (car lst)) (loop (cdr lst)))
          (else #f))))

(define every? every)

;;=============================================================================
;; List Transformation & Iteration
;;=============================================================================

;; Tail-recursive filter.
(define (filter pred lst)
  (let loop ((lst lst) (acc '()))
    (cond ((null? lst) (reverse acc))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc)))
          (else (loop (cdr lst) acc)))))

;; Left-associative fold.
(define (fold proc seed lst)
  (let loop ((lst lst) (acc seed))
    (if (null? lst)
        acc
        (loop (cdr lst) (proc (car lst) acc)))))

;; Returns a list of integers from 0 to n-1.
(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;; Partition lst into two lists: those that satisfy pred and those that do not.
(define (partition pred lst)
  (let loop ((lst lst) (in '()) (out '()))
    (cond ((null? lst) (values (reverse in) (reverse out)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) in) out))
          (else (loop (cdr lst) in (cons (car lst) out))))))

;; Returns the first n elements of lst.
(define (take-elements lst n)
  (let loop ((lst lst) (n n) (acc '()))
    (if (or (<= n 0) (null? lst))
        (reverse acc)
        (loop (cdr lst) (- n 1) (cons (car lst) acc)))))

(define take take-elements)
(define list-head take-elements)

;; Returns the tail of lst starting after the first n elements.
(define (drop-elements lst n)
  (let loop ((lst lst) (n n))
    (if (or (<= n 0) (null? lst))
        lst
        (loop (cdr lst) (- n 1)))))

(define drop drop-elements)
(define list-tail drop-elements)

;; Join a list of strings with a delimiter.
(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (let loop ((rest (cdr strings)) (acc (car strings)))
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc delimiter (car rest)))))))

;; Map a function over a potentially improper list.
(define (map-improper func lst)
  (cond ((null? lst) '())
        ((pair? lst) (cons (func (car lst)) (map-improper func (cdr lst))))
        (else (func lst))))

;; Efficiently flatten nested 'begin' forms.
(define (flatten-begins exprs)
  (reverse
   (let loop ((input exprs) (acc '()))
     (cond ((null? input) acc)
           ((pair? input)
            (let ((head (car input)))
              (if (and (pair? head) (eq? (car head) 'begin))
                  (loop (cdr input) (loop (cdr head) acc))
                  (loop (cdr input) (cons head acc)))))
           (else (cons input acc))))))

;; Remove items from a list (uses memq for comparison).
(define (remove-from-list lst remove-items)
  (let loop ((lst lst) (acc '()))
    (cond ((null? lst) (reverse acc))
          ((memq (car lst) remove-items) (loop (cdr lst) acc))
          (else (loop (cdr lst) (cons (car lst) acc))))))

;; Delete all occurrences of x in lst (uses equal? for comparison).
(define (delete x lst)
  (let loop ((lst lst) (acc '()))
    (cond ((null? lst) (reverse acc))
          ((equal? x (car lst)) (loop (cdr lst) acc))
          (else (loop (cdr lst) (cons (car lst) acc))))))

;;=============================================================================
;; Set Operations
;;=============================================================================

;; Union of multiple sets (lists treated as sets).
(define (set-union . sets)
  (let loop ((sets sets) (acc '()))
    (if (null? sets)
        (reverse acc)
        (loop (cdr sets)
              (let inner ((s (car sets)) (a acc))
                (cond ((null? s) a)
                      ((memq (car s) a) (inner (cdr s) a))
                      (else (inner (cdr s) (cons (car s) a)))))))))

;; Set difference: s1 minus s2.
(define (set-minus s1 s2)
  (filter (lambda (x) (not (memq x s2))) s1))

;; Pattern matcher for recursive let (loops)
;; Matches: (let ((<name> #f)) (set! <name> (lambda ...)) <name>)
(define (match-rec-pattern expr)
  (let ((match?
         (lambda (expr)
           (and (pair? expr)
                (eq? (car expr) 'let)
                (let ((bindings (cadr expr))
                      (body (cddr expr)))
                  (and (pair? bindings)
                       (null? (cdr bindings))
                       (let ((name (car (car bindings))))
                         (and (symbol? name)
                              (pair? body)
                              (let ((first (car body)))
                                (and (pair? first)
                                     (eq? (car first) 'set!)
                                     (eq? (cadr first) name)
                                     (pair? (cddr first))
                                     (let ((val (caddr first)))
                                       (and (pair? val)
                                            (eq? (car val) 'lambda)))))))))))))
    (match? expr)))

(define (generate-temporary-symbol prefix)
  (set! *syntax-temp-counter* (+ *syntax-temp-counter* 1))
  (string->symbol (string-append prefix (number->string *syntax-temp-counter*))))
