(define-module (core)
  (export c-callback
          c-function
          c-function/weak
          current-exception-handler
          define-struct
          destructuring-bind
          destructuring-match
          do
          let*-values
          let-values
          load-shared-object
          lookup-shared-object
          parameterize
          pretty-print
          pretty-print-initial-indent
          pretty-print-line-length
          pretty-print-maximum-lines
          raise
          raise-continuable
          unless
          when
          with-exception-handler)
  (import (core base)
          (core cffi)
          (core destructuring)
          (core exception)
          (core let-values)
          (core parameterize)
          (core pretty-print)
          (core struct)))