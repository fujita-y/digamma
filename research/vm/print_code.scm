
(use srfi-1)
(load "compiler.scm")

(define expr '(begin
                 (define fact (lambda (n acc)
                                (if (= n 0) acc
                                    (fact (- n 1) (* n acc)))))
                 (fact 5 1)))

(define code (compile expr))
(for-each (lambda (i) (print i)) (vector->list code))
