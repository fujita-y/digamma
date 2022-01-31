#!nobacktrace
(define-library (srfi 9)
  (import (core))
  (export (rename (define-record-type/srfi-9 define-record-type)))
  (begin
    (define-syntax define-record-type/srfi-9
      (lambda (x)
        (define parse-spec
          (lambda (spec)
            (syntax-case spec ()
              ((x y) (syntax (immutable x y)))
              ((x y z) (syntax (mutable x y z))))))
        (syntax-case x ()
          ((_ type (ctor constructor-tag ...) pred spec ...)
           (with-syntax ((((field-tag _ ...) ...)
                          (syntax (spec ...)))
                         ((r6rs:field-spec ...)
                          (map parse-spec (syntax (spec ...)))))
             (let ((ctags (syntax (constructor-tag ...))))
               (with-syntax (((constructor-arg ...)
                              (map (lambda (ftag)
                                     (if (find (lambda (e) (bound-identifier=? e ftag)) ctags)
                                         ftag
                                         (syntax (unspecified))))
                                   (syntax (field-tag ...)))))
                 (syntax
                   (define-record-type
                     (type ctor pred)
                     (protocol
                       (lambda (ctor)
                         (lambda (constructor-tag ...)
                           (ctor constructor-arg ...))))
                     (fields r6rs:field-spec ...))))))))))))
