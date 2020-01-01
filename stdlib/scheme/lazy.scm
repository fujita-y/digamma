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
   (define delay)
   (define force)
   (define promise?)
   (define delay-force)
   (define make-promise)
  )
) ;[end]
