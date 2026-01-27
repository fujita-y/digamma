;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Common utilities for syntax expansion system.

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (every? pred lst)
  (or (null? lst) (and (pred (car lst)) (every? pred (cdr lst)))))

(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst)) (any? pred (cdr lst)))))

(define (iota n)
  (let loop ((i 0))
    (if (= i n) '() (cons i (loop (+ i 1))))))

(define (take lst n)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

(define (map-improper func lst)
  (cond
    ((null? lst) '())
    ((pair? lst) (cons (func (car lst)) (map-improper func (cdr lst))))
    (else (func lst))))

(define (flatten-begins exprs)
  (let loop ((exprs exprs))
    (cond ((null? exprs) '())
          ((pair? exprs)
           (let ((first (car exprs)))
             (if (and (pair? first) (eq? (car first) 'begin))
                 (append (loop (cdr first)) (loop (cdr exprs)))
                 (cons first (loop (cdr exprs))))))
          (else (list exprs)))))

(define (remove-from-list lst remove-items)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (loop (cdr lst) (if (memq (car lst) remove-items) acc (cons (car lst) acc))))))
