(import (digamma c-ffi))
(import (digamma concurrent))

(define sqrt (c-function/weak double sqrt (double)))

;(sqrt 100)

(define n (await (async (sqrt 100))))
n

;;;

(import (digamma c-ffi))
(import (digamma concurrent))

(define sqrt (c-function double sqrt (double)))

;(sqrt 100)

(define n (await (async (sqrt 100))))
n
