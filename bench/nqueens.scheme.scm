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

(define wait-codegen-idle
  (lambda ()
    (let loop ()
      (usleep 100000)
      (cond ((= (codegen-queue-count) 0))
            (else (loop))))))

;; warmup and wait for codegen queue empty
(nqueens 8) (collect) (wait-codegen-idle)

(time (nqueens 12))
;; interpreted => 1.110873 real    1.146198 user    0.000000 sys (#define ENABLE_LLVM_JIT 0)
;; jit enabled => 0.515772 real    0.881979 user    0.008091 sys (#define ENABLE_LLVM_JIT 1)
