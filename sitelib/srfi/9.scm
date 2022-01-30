#!nobacktrace
(define-library (srfi 9)
  (import (core))
  (export (rename (define-record-type/srfi-9 define-record-type)))
  (begin
    (define-syntax define-record-type/srfi-9
      (lambda (x)
        (define parse
          (lambda (stx)
            (syntax-case stx
              ()
              ((x y) (syntax (immutable x y)))
              ((x y z) (syntax (mutable x y z))))))
        (syntax-case x
          ()
          ((_ type (ctor _ ...) pred spec ...)
           (with-syntax (((spec ...) (map parse (syntax (spec ...)))))
             (syntax (define-record-type (type ctor pred) (fields spec ...))))))))))
