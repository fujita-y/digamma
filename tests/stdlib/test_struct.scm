(import (core test-lite))

;; -------------------------------------
;; (core struct)
;; -------------------------------------
(test-begin "core struct")

(test-eval! (import (core struct)))

(test-eval!
 (define-struct test-struct (x y)))

(test-equal "struct predicate from core struct" #t
  (let ((p (make-test-struct 1 2)))
    (test-struct? p)))

(test-equal "struct accessors from core struct" (1 2)
  (let ((p (make-test-struct 1 2)))
    (list (test-struct-x p) (test-struct-y p))))

(test-equal "struct setters from core struct" (3 4)
  (let ((p (make-test-struct 1 2)))
    (test-struct-x-set! p 3)
    (test-struct-y-set! p 4)
    (list (test-struct-x p) (test-struct-y p))))

(test-end)

(test-report)
(exit)
