(load "./research/vm/compiler.scm")

(define (print-code code)
  (for-each (lambda (i) (print i)) (vector->list code)))
