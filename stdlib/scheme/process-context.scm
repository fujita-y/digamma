
;;; Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library
  (scheme process-context)
  (import (rename (core) (exit core:exit)))
  (export command-line
          exit
          get-environment-variable
          get-environment-variables
          emergency-exit)
  (begin
    (define get-environment-variable lookup-process-environment)
    (define get-environment-variables process-environment->alist)
    (define emergency-exit core:exit)
    (define exit #f)
  )
) ;[end]
