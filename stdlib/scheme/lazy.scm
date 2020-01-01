;;; Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library
  (scheme lazy)
  (import (rnrs))
  (export delay
          force
          promise?
          delay-force
          make-promise)
  (begin
   (define delay #f)
   (define force #f)
   (define promise? #f)
   (define delay-force #f)
   (define make-promise #f)
  )
) ;[end]

;; https://nacl-ltd.github.io/2016/05/31/delay-force.html


;; http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html

; type Promise a = lazy (Promise a) | eager a

(define make-promise
  (lambda (obj)
    (tuple 'type:promise (cons #t obj))))

(define promise?
  (lambda (obj)
    (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:promise))))

(define promise-done?
  (lambda (obj)
    (if (promise? obj) (car (tuple-ref obj 1)) #t)))

(define promise-value
  (lambda (obj)
    (if (promise? obj) (cdr (tuple-ref obj 1)) obj)))

(define force
  (lambda (promise)
    (if (promise-done? promise)
        (promise-value promise)
        (let ((value ((promise-value promise))))
          (cond ((promise-done? promise) (promise-value promise))
                ((promise-done? value) (promise-value value))
                (else
                  (tuple-set! promise 1 (tuple-ref value 1))
                  ;(set-car! (tuple-ref promise 1) (promise-done? new))
                  ;(set-cdr! (tuple-ref promise 1) (promise-value new))
                  ;(tuple-set! new 1 (tuple-ref promise 1))
                  (force promise)))))))

(define-syntax delay-force
  (syntax-rules ()
    ((_ expr)
     (tuple 'type:promise (cons #f (lambda () expr))))))

(define-syntax delay
  (syntax-rules ()
    ((_ expr)
     (delay-force (make-promise expr)))))


(force (delay (+ 3 4)))

  (force (delay expression)) -> expression
  (force (delay-force (+ 3 4))) -> (force expression)
  (force (delay (force (+ 3 4))))
  (force (eager value))      -> value

;;;; ====

(define make-promise
  (lambda (obj)
    (tuple 'type:promise (cons #t obj))))

(define promise?
  (lambda (obj)
    (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:promise))))

(define promise-done?
  (lambda (obj)
    (or (not (promise? obj))
        (car (tuple-ref obj 1)))))

(define promise-value
  (lambda (obj)
    (if (promise? obj)
        (cdr (tuple-ref obj 1))
        obj)))

(define force
  (lambda (promise)
    (if (promise-done? promise)
        (promise-value promise)
        (let ((new ((promise-value promise))))
          (cond ((promise-done? promise)
                 (promise-value promise))
                ((promise? new)
                  (set-car! (tuple-ref promise 1) (promise-done? new))
                  (set-cdr! (tuple-ref promise 1) (promise-value new))
                  (tuple-set! new 1 (tuple-ref promise 1))
                  (force promise))
                (else
                  new))))))

(define-syntax delay-force
  (syntax-rules ()
    ((_ expr)
     (tuple 'type:promise (cons #f (lambda () expr))))))

(define-syntax delay
  (syntax-rules ()
    ((_ expr)
     (delay-force (make-promise expr)))))

;;;; ====
lazy == delay-force

(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
     (box (cons 'lazy (lambda () exp))))))

(define (eager x)
  (box (cons 'eager x)))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lazy (eager exp)))))

(define (force promise)
  (let ((content (unbox promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))
                      (content  (unbox promise)))                      ; *
                 (if (not (eqv? (car content) 'eager))                 ; *
                     (begin (set-car! content (car (unbox promise*)))
                            (set-cdr! content (cdr (unbox promise*)))
                            (set-box! promise* content)))
                 (force promise))))))