;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Common utilities for the Digamma core system.
;; These functions provide a robust base for syntax expansion, optimization, and compilation.

(cond-expand
  (gauche
   (define (make-eq-hashtable) (make-hash-table 'eq?))
   (define (make-hashtable hash-fn equiv-fn)
     (cond ((eq? equiv-fn equal?) (make-hash-table 'equal?))
           ((eq? equiv-fn eqv?) (make-hash-table 'eqv?))
           ((eq? equiv-fn eq?) (make-hash-table 'eq?))
           (else (make-hash-table equiv-fn))))
   (define hashtable-clear! hash-table-clear!)
   (define hashtable-contains? hash-table-exists?)
   (define hashtable-ref hash-table-get)
   (define hashtable-set! hash-table-put!)
   (define hashtable->alist hash-table->alist)
   (define (equal-hash obj) (hash obj)))
  (ypsilon
   (define make-eq-hashtable (let ((val make-eq-hashtable)) val))
   (define make-hashtable (let ((val make-hashtable)) val))
   (define hashtable-clear! (let ((val hashtable-clear!)) val))
   (define hashtable-contains? (let ((val hashtable-contains?)) val))
   (define hashtable-ref (let ((val hashtable-ref)) (lambda (ht key . opt) (if (null? opt) (val ht key #f) (val ht key (car opt))))))
   (define hashtable-set! (let ((val hashtable-set!)) val))
   (define (hashtable->alist ht)
     (let-values (((keys vals) (hashtable-entries ht)))
       (map cons (vector->list keys) (vector->list vals))))
   (define (equal-hash obj) (equal-hash obj)))
  (else))

;;=============================================================================
;; Globals & State
;;=============================================================================

;; Counter for generating unique temporary symbol names
(define *syntax-temp-counter* 0)

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

;; Standard partition implementation.
(define (partition pred lst)
  (let loop ((lst lst) (in '()) (out '()))
    (cond ((null? lst) (values (reverse in) (reverse out)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) in) out))
          (else (loop (cdr lst) in (cons (car lst) out))))))

;; Take first n elements of a list.
(define (take lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define list-head take)

;; Drop first n elements of a list.
(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

(define list-tail drop)

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

(define (generate-temporary-symbol prefix)
  (set! *syntax-temp-counter* (+ *syntax-temp-counter* 1))
  (string->symbol (string-append prefix (number->string *syntax-temp-counter*))))
