(import (core test-lite))

;; -------------------------------------
;; (core parameterize)
;; -------------------------------------
(test-begin "core parameterize")

(test-eval! (import (core parameterize)))

(test-equal "parameterize simple binding" 2
  (let ((p (make-parameter 1)))
    (parameterize ((p 2))
      (p))))

(test-equal "parameterize multiple bindings" (2 3)
  (let ((p (make-parameter 1))
        (q (make-parameter 2)))
    (parameterize ((p 2) (q 3))
      (list (p) (q)))))

(test-equal "parameterize restores original values" (1 2)
  (let ((p (make-parameter 1))
        (q (make-parameter 2)))
    (parameterize ((p 3) (q 4))
      (list (p) (q)))
    (list (p) (q))))

(test-end)

(test-report)
(exit)
