#!nobacktrace
;;; Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma concurrent)
  (export define-thread-variable
          define-autoload-variable
          async
          awaitable?
          await
          pmap
          make-uuid
          make-shared-queue
          shared-queue?
          shared-queue-push!
          shared-queue-pop!
          shared-queue-shutdown
          serializable?
          timeout-object?
          shutdown-object?
          spawn
          spawn*
          spawn-timeout
          spawn-heap-limit
          current-exception-printer)
  (import (core))

  (define-syntax define-thread-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define param (make-parameter (list init)))
         (define mutator (lambda (val) (param (list val))))
         (define accessor
           (lambda ()
             (let ((p (param)))
               (if (local-heap-object? p) (car p) (let ((val init)) (param (list val)) val)))))
         (define-syntax var (identifier-syntax (_ (accessor)) ((set! _ x) (mutator x))))))))

  (define-syntax define-autoload-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define undefined (list #f))
         (define param (make-parameter undefined))
         (define accessor
           (lambda ()
             (if (on-primordial-thread?)
                 (let ((val init)) (set! accessor (lambda () val)) val)
                 (let ((val (param)))
                   (if (not (eq? val undefined)) val (let ((val init)) (param val) val))))))
         (define-syntax var
           (identifier-syntax
             (_ (accessor))
             ((set! var x)
              (assertion-violation
                'set!
                (format "attempt to modify autoload variable ~u" 'var)
                '(set! var x)))))))))

  (define-syntax async
    (syntax-rules ()
      ((_ e0 e1 ...)
       (let ((promise (make-shared-queue)) (completed #f) (result #f))
         (spawn*
           (lambda () e0 e1 ...)
           (lambda (ans) (shared-queue-push! promise ans)))
         (tuple
          'type:awaitable
           (lambda timeout
             (cond (completed (if (condition? result) (raise result) result))
                   (else
                     (set! result (apply shared-queue-pop! promise timeout))
                     (cond ((timeout-object? result) result)
                           (else
                             (set! completed #t)
                             (if (condition? result) (raise result) result)))))))))))

  (define awaitable?
    (lambda (obj)
      (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:awaitable))))

  (define await
    (lambda (x . timeout)
      (or (awaitable? x)
          (assertion-violation 'await (format "expected awaitable, but got ~r, as argument 1" x) (cons x timeout)))
      (apply (tuple-ref x 1) timeout)))

  (define pmap
    (lambda (proc lst1 . lst2)
      (define pmap-1
        (lambda (proc lst)
          (map await
            (map (lambda (arg) (async (proc arg))) lst))))
      (define pmap-n
        (lambda (proc lst)
          (map await
            (map (lambda (args) (async (apply proc args))) lst))))
      (if (null? lst2)
          (if (list? lst1)
              (pmap-1 proc lst1)
              (assertion-violation 'pmap "expected proper lists" (cons* proc lst1 lst2)))
          (cond ((apply list-transpose+ lst1 lst2) => (lambda (lst) (pmap-n proc lst)))
                (else (assertion-violation 'pmap "expected same length proper lists" (cons* proc lst1 lst2)))))))

  (define spawn*
    (lambda (body finally)
      (spawn
        (lambda ()
          (finally
            (call/cc
              (lambda (escape)
                (with-exception-handler (lambda (c) (escape c)) (lambda () (body))))))))))


  ) ;[end]

#|

(import (digamma concurrent))
(import (digamma time))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (test-para)
  (pmap fib '(40 40 40 40 40 40 40 40))) ; x8

(time (test-para))

(define (test-plain)
   (map fib '(40 40 40 40 40 40 40 40))) ; x8

(time (fib 40))
;;  8.342836 real    8.339067 user    0.002708 sys

(time (test-plain))
;; 65.820770 real   65.808054 user    0.014191 sys

(time (test-para))
(time (let ((a (async (test-para)))) (await a)))
;; 15.660831 real  122.926724 user    0.069117 sys

(import (digamma concurrent))
(import (digamma time))

(pmap list '(1 2 3) '(4 5 6))

(pmap exit '(1 2 3))
(begin (async (fib 40)) (exit))

////

(import (digamma concurrent))
(import (digamma time))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))


(define (test-para)
  (pmap ack '(3 3 3 3 3 3 3 3) '(9 9 9 9 9 9 9 9)))

(time (test-para))

|#
