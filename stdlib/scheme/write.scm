;;; Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library
  (scheme write)
  (import (core))
  (export display write-shared write write-simple)
  (begin
    (define write-shared write-with-shared-structure)
    (define write-simple write)
  )
) ;[end]
