;; ./digamma --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/codegen.scm

(import (core) (test-lite))

(display "test native code generation\n\n")

(test-begin "fib")
(test-eval!
  (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
        (fib (- n 2))))))
(test-eval!
  (closure-compile fib))
(test-equal "(fib 30)"
  (fib 30) => 832040)
(test-end)

(test-begin "map-1")
(test-eval!
  (define (minus x) (- x)))
(test-eval!
  (define lst (make-list 10 '4)))
(test-eval!
  (define map-1
    (lambda (proc lst)
      (if (null? lst)
          '()
          (cons (proc (car lst))
                (map-1 proc (cdr lst)))))))
(test-eval!
  (closure-compile map-1))
(test-equal "(map-1 minus lst)"
  (map-1 minus lst) => (-4 -4 -4 -4 -4 -4 -4 -4 -4 -4))
(test-end)

(test-begin "lambda")
(test-eval!
  (define (n m) (list 1 (m) 3)))
(test-eval!
  (closure-compile n))
(test-equal "(n (lambda () 2))" (n (lambda () 2)) => (1 2 3))
(test-end)

(test-begin "continuation")
(test-eval!
  (define c #f))
(test-eval!
  (define (n m) (list 1 (m) 3)))
(test-eval!
  (closure-compile n))
(test-eval!
  (n (lambda () (call/cc (lambda (k) (set! c k) 2))))) ; => (1 2 3)
(test-equal "(c 1000)"
  (c 1000) => (1 1000 3))
(test-end)

(test-begin "map-n")
(test-eval!
  (define map-1
    (lambda (proc lst)
      (cond ((null? lst) '())
            (else (cons (proc (car lst))
                        (map-1 proc (cdr lst))))))))
(test-eval!
  (define map-n
    (lambda (proc lst)
      (cond ((null? lst) '())
            (else (cons (apply proc (car lst))
                        (map-n proc (cdr lst))))))))
(test-eval!
  (define map
    (lambda (proc lst1 . lst2)
      (if (null? lst2)
          (map-1 proc lst1)
          (map-n proc (apply list-transpose* lst1 lst2))))))
(test-eval!
  (begin
    (closure-compile map-1)
    (closure-compile map-n)
    (closure-compile map)))
(test-equal "(map cons '(1 2) '(3 4))"
  (map cons '(1 2) '(3 4)) => ((1 . 3) (2 . 4)))
(test-end)

(test-begin "stackable closure")
(test-eval!
  (define acc #f))
(test-eval!
  (define add (lambda (n) (set! acc (cons n acc)))))
(test-eval!
  (define (p n m)
    (let loop1 ((n n))
      (cond ((> n 2))
            (else
              (let loop2 ((m m))
                (cond ((> m 2))
                      (else
                        (add n)
                        (add m)
                      (loop2 (+ m 1)))))
              (loop1 (+ n 1)))))))
(test-eval!
  (p 0 0))
(test-eval!
  (closure-compile p))
(test-equal "acc"
  acc => (2 2 1 2 0 2 2 1 1 1 0 1 2 0 1 0 0 0 . #f))
(test-end)


;; ./digamma --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/codegen.scm
