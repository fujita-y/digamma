#!nobacktrace
(library (srfi 8)
  (export receive)
  (import (core))
  (define-syntax receive
    (syntax-rules ()
      ((_ formals expr body ...)
       (let-values ((formals expr)) body ...)))))
