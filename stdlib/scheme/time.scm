;;; Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library
  (scheme time)
  (import (only (core) microsecond) (scheme base))
  (export current-jiffy jiffies-per-second current-second)
  (begin
    (define current-jiffy microsecond)
    (define jiffies-per-second (lambda () 1000000))
    (define current-second (lambda () (/ (microsecond) 1000000.0)))
  )
) ;[end]
