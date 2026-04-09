;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-module (core exception)

  (export current-exception-handler with-exception-handler raise raise-continuable)

  (import (core parameterize))

  (define current-exception-handler
    (make-parameter (lambda (x) (assertion-violation #f (format "unhandled exception: ~s" x)))))

  (define parent-exception-handler
    (make-parameter (lambda (x) (assertion-violation #f (format "unhandled exception: ~s" x)))))

  (define (with-exception-handler handler thunk)
    (parameterize ((parent-exception-handler (current-exception-handler))
                   (current-exception-handler handler)) 
      (with-cpp-exception-handler handler thunk)))

  (define (raise x) 
    ((current-exception-handler) x)
    ((parent-exception-handler) "raise: returned from non-continuable exception"))

  (define (raise-continuable x) 
    ((current-exception-handler) x))

)
