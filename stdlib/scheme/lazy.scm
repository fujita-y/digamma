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
