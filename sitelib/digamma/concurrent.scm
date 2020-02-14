#!nobacktrace
;;; Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma concurrent)
  (export define-thread-variable
          define-autoload-variable
          make-uuid
          async
          make-shared-queue
          shared-queue?
          shared-queue-push!
          shared-queue-pop!
          shared-queue-shutdown
          make-shared-bag
          shared-bag?
          shared-bag-put!
          shared-bag-get!
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
               (if (local-heap-object? p)
                   (car p)
                   (let ((val init))
                     (param (list val)) val)))))
         (define-syntax var
           (identifier-syntax
             (_ (accessor))
             ((set! _ x) (mutator x))))))))

  (define-syntax define-autoload-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define undefined (list #f))
         (define param (make-parameter undefined))
         (define accessor
           (lambda ()
             (if (on-primordial-thread?)
                 (let ((val init))
                   (set! accessor (lambda () val)) val)
                 (let ((val (param)))
                   (if (not (eq? val undefined))
                       val
                       (let ((val init))
                         (param val) val))))))
         (define-syntax var
           (identifier-syntax
             (_ (accessor))
             ((set! var x)
              (assertion-violation 'set! (format "attempt to modify autoload variable ~u" 'var) '(set! var x)))))))))

  (define-syntax async
    (syntax-rules ()
      ((_ e0 e1 ...)
       (let ((queue (make-shared-queue)) (completed #f) (value #f))
         (spawn*
          (lambda () e0 e1 ...)
          (lambda (ans)
            (shared-queue-push! queue ans)
            (shared-queue-shutdown queue)))
         (lambda timeout
           (cond
            (completed
             (if (condition? value) (raise value) value))
            (else
             (set! value (apply shared-queue-pop! queue timeout))
             (cond
              ((timeout-object? value) value)
              (else
               (set! completed #t)
               (if (condition? value) (raise value) value))))))))))

  (define spawn*
    (lambda (body finally)
      (spawn (lambda ()
               (finally
                (call/cc
                 (lambda (escape)
                   (with-exception-handler
                    (lambda (c) (escape c))
                    (lambda () (body))))))))))

  ) ;[end]

#|
(import (digamma concurrent))
(define f (async (list 1 2 3)))
(f)
(define f (async (list 1 2 3) (usleep 10000) #t))
(f 1)
(display-thread-status)
(define f2 (async (list 1 a 3)))
(f2)

|#