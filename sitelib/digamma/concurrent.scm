#!nobacktrace
;;; Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma concurrent)
  (export define-thread-variable
          define-autoload-variable
          make-uuid
          future
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

  (define future-error
    (condition
     (make-error)
     (make-who-condition 'future)
     (make-message-condition "child thread has terminated by unhandled exception")))

  (define-syntax future
    (syntax-rules ()
      ((_ e0 e1 ...)
       (let ((queue (make-shared-queue)))
         (spawn*
          (lambda () e0 e1 ...)
          (lambda (ans)
            (if (condition? ans)
                (shared-queue-push! queue future-error)
                (shared-queue-push! queue ans))
            (shared-queue-shutdown queue)))
         (lambda timeout
           (let ((ans (apply shared-queue-pop! queue timeout)))
             (if (condition? ans) (raise ans) ans)))))))

  (define spawn*
    (lambda (body finally)

      (define print-exception
        (lambda (c)
          (let ((e (current-error-port)))
            (format e "\nerror in thread: unhandled exception has occurred\n")
            (let ((in (open-string-input-port
                       (call-with-string-output-port
                        (lambda (s)
                          (set-current-error-port! s)
                          ((current-exception-printer) c)
                          (set-current-error-port! e))))))
              (or (char=? (lookahead-char in) #\linefeed) (put-string e "  "))
              (let loop ((ch (get-char in)))
                (cond ((eof-object? ch)
                       (format e "[thread exit]\n\n"))
                      ((char=? ch #\linefeed)
                       (put-string e "\n  ")
                       (loop (get-char in)))
                      (else
                       (put-char e ch)
                       (loop (get-char in)))))))))

      (spawn (lambda ()
               (finally
                (call/cc
                 (lambda (escape)
                   (with-exception-handler
                    (lambda (c)
                      (print-exception c)
                      (and (serious-condition? c) (escape c)))
                    (lambda () (body))))))))))

  ) ;[end]
