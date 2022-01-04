#!core
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (core conditions)

  (export define-condition-type
          condition simple-conditions condition?
          condition-predicate condition-accessor
          &condition
          &message make-message-condition message-condition? condition-message
          &warning make-warning warning?
          &serious make-serious-condition serious-condition?
          &error make-error error?
          &violation make-violation violation?
          &assertion make-assertion-violation assertion-violation?
          &irritants make-irritants-condition irritants-condition? condition-irritants
          &who make-who-condition who-condition? condition-who
          &non-continuable make-non-continuable-violation non-continuable-violation?
          &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
          &lexical make-lexical-violation lexical-violation?
          &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
          &undefined make-undefined-violation undefined-violation?)

  (import (core primitives)
          (core syntax-case)
          (core records))

  (define-syntax define-condition-type
    (lambda (x)
      (syntax-case x ()
        ((_ condition-type supertype
            constructor predicate
            (cond-fields cond-accessors) ...)
         (with-syntax (((rec-accessors ...) (generate-temporaries (syntax (cond-fields ...)))))
           (syntax (begin
                     (define-record-type (condition-type constructor temp)
                       (parent supertype)
                       (nongenerative)
                       (fields (immutable cond-fields rec-accessors) ...))
                     (define predicate
                       (condition-predicate (record-type-descriptor condition-type)))
                     (define cond-accessors
                       (condition-accessor (record-type-descriptor condition-type) rec-accessors)) ...)))))))

  ) ;[end]
