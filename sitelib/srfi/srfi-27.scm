#!nobacktrace
(define-library (srfi srfi-27)
  (import (srfi 27))
  (export default-random-source
          make-random-source
          random-integer
          random-real
          random-source-make-integers
          random-source-make-reals
          random-source-pseudo-randomize!
          random-source-randomize!
          random-source-state-ref
          random-source-state-set!
          random-source?))
