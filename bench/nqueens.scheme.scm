(import (digamma time))

;; nqueens from Gambit benchmark
(define (nqueens n)
  (define (|1-to| n) (let loop ((i n) (l '())) (if (= i 0) l (loop (- i 1) (cons i l)))))
  (define (my-try x y z)
    (if (null? x)
        (if (null? y) 1 0)
        (+
          (if (ok? (car x) 1 z) (my-try (append (cdr x) y) '() (cons (car x) z)) 0)
          (my-try (cdr x) (cons (car x) y) z))))
  (define (ok? row dist placed)
    (if (null? placed)
        #t
        (and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (ok? row (+ dist 1) (cdr placed)))))
  (my-try (|1-to| n) '() '()))

;; warmup
(nqueens 8) (collect) (usleep 2000000)

(time (nqueens 12))
;; interpreted => 2.867542 real    2.972887 user    0.003328 sys (#define ENABLE_LLVM_JIT 0)
;; jit enabled => 0.819963 real    0.930319 user    0.002824 sys (#define ENABLE_LLVM_JIT 1)
