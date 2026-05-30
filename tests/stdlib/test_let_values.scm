(import (core test-lite))

;; -------------------------------------
;; (core let-values)
;; -------------------------------------
(test-begin "core let-values")

(test-eval! (import (core let-values)))

(test-equal "let-values single binding" 3
  (let-values (((x y) (values 1 2)))
    (+ x y)))

(test-equal "let-values one-element values" 42
  (let-values (((x) (values 42)))
    x))

(test-equal "let-values multiple bindings" (1 2 3 4)
  (let-values (((x y) (values 1 2))
               ((a b) (values 3 4)))
    (list x y a b)))

(test-equal "let*-values sequential bindings" 36
  (let*-values (((x y) (values 1 2))
                ((z w) (values (+ x 10) (+ y 20))))
    (+ x y z w)))

(test-end)

(test-report)
(exit)
