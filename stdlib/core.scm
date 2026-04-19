(define-module (core)
  (export do
          when
          unless
          pretty-print
          destructuring-match
          destructuring-bind
          current-exception-handler
          with-exception-handler
          raise
          raise-continuable
          let-values
          let*-values
          parameterize
          load-shared-object
          lookup-shared-object
          c-function
          c-function/weak
          c-callback)
  (import (core base)
          (core pretty-print)
          (core destructuring)
          (core exception)
          (core let-values)
          (core parameterize)
          (core cffi)))