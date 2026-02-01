;; use chezscheme or ypsilon to run this file

(load "r7rs_library.scm")

;; --- Library A ---
(r7rs:define-library (lib A)
  (export a val-a)
  (begin
    (define (a x) (list 'A x))
    (define val-a 100)))

;; --- Library B ---
(r7rs:define-library (lib B)
  (import (lib A))
  (export b val-b)
  (begin
    ;; Use imported 'a' and 'val-a'
    (define (b x) (cons 'B (a x)))
    (define val-b (+ val-a 50))))

;; --- Library C (renaming) ---
(r7rs:define-library (lib C)
  (import (rename (lib A) (a a-renamed)))
  (export c)
  (begin
    (define (c x) (list 'C (a-renamed x)))))

;; --- Library D (shadowing) ---
(r7rs:define-library (lib D)
  (export d)
  (begin
    (define (d-shadowed-1 x) (list 'D 'shadowed-1 x))
    (define (d-shadowed-2 x) (list 'D 'shadowed-2 x))
    (define d d-shadowed-2)))

(define d 100)
(define d-shadowed-1 1000)

;; --- Top Level Test ---
(r7rs:import (lib B))
(r7rs:import (prefix (lib C) C:))
(r7rs:import (lib D))

(display (b 1)) (newline)      ;; Should be (B A 1)
(display val-b) (newline)      ;; Should be 150
(display (C:c 2)) (newline)    ;; Should be (C A 2)
(display (d 3)) (newline)      ;; Should be (D shadowed-2 3)
(display d-shadowed-1) (newline) ;; Should be 1000
(display d-shadowed-2) (newline) ;; Should be unbound variable error
