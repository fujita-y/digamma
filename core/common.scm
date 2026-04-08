;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Common utilities

;;=============================================================================
;; Globals & State
;;=============================================================================

;; Counter for generating unique suffixes to ensure hygiene during renaming
(define *suffix-counter* 0)

;; Counter for generating unique marks for macro transformations
(define *mark-counter* 0)

;; Generate a fresh unique suffix for identifier renaming.
(define (fresh-suffix)
  (set! *suffix-counter* (+ *suffix-counter* 1))
  (number->string *suffix-counter*))

;; Generate a fresh unique mark for macro transformations.
(define (fresh-mark)
  (set! *mark-counter* (+ *mark-counter* 1))
  *mark-counter*)

;; Returns #t if x is a proper list, #f otherwise.
(define (proper-list? x)
  (let loop ((x x))
    (cond ((null? x) #t)
          ((pair? x) (loop (cdr x)))
          (else #f))))

;; Returns the first n elements of lst.
(define (list-head lst n)
  (let loop ((lst lst) (n n) (acc '()))
    (if (or (<= n 0) (null? lst))
        (reverse acc)
        (loop (cdr lst) (- n 1) (cons (car lst) acc)))))

;; Returns the tail of lst starting after the first n elements.
(define (list-tail lst n)
  (let loop ((lst lst) (n n))
    (if (or (<= n 0) (null? lst))
        lst
        (loop (cdr lst) (- n 1)))))

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
  (if (eq? (environment-macro-ref 'begin) 'builtin)
      (reverse
        (let loop ((input exprs) (acc '()))
          (cond ((null? input) acc)
                ((pair? input)
                 (let ((head (car input)))
                   (if (and (pair? head) (eq? (car head) 'begin))
                       (loop (cdr input) (loop (cdr head) acc))
                       (loop (cdr input) (cons head acc)))))
                (else (cons input acc)))))
      exprs))

;; Remove items from a list.
(define (remove-from-list lst remove-items)
  (let ((ht (make-eq-hashtable)))
    (for-each (lambda (x) (hashtable-set! ht x #t)) remove-items)
    (filter (lambda (x) (not (hashtable-contains? ht x))) lst)))

(define (generate-temporary-symbol prefix) (gensym prefix))
