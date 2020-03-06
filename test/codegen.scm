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

(test-begin "local closure (loop)")
(test-eval!
  (define acc #f))
(test-eval!
  (define add (lambda (n) (set! acc (cons n acc)))))
(test-eval!
  (define (p m)
      (let loop ((n 0))
        (add n)
        (cond ((> n m) #f)
              (else
                (add n)
                (loop (+ n 1)))))))
(test-eval!
  (closure-compile p))
(test-eval!
  (p 4))
(test-equal "acc"
  acc => (5 4 4 3 3 2 2 1 1 0 0 . #f))
(test-end)

(test-begin "stackable closure (nest loop)")
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
  (closure-compile p))
(test-eval!
  (p 0 0))
(test-equal "acc"
  acc => (2 2 1 2 0 2 2 1 1 1 0 1 2 0 1 0 0 0 . #f))
(test-end)

(test-begin "subr partial use of args")
(test-eval!
  (define acc #f))
(test-eval!
  (define add (lambda (n) (set! acc (cons n acc)))))
(test-eval!
  (define (p name count ok? run)
    (let loop ((i 0) (result (list 'undefined)))
      (if (< i count)
          (loop (+ i 1) (run i))
          result))))
(test-eval!
  (closure-compile p))
(test-eval!
  (p "foo" 3 #t add))
(test-equal "acc"
  acc => (2 1 0 . #f))
(test-end)

(test-begin "subr violation 1")
(test-eval!
  (begin
    (define (p) (+ 1 'o))
    (closure-compile p)))
(test-assertion-violation "subr violation 1" (p))
(test-end)

(test-begin "subr violation 2")
(test-eval!
  (begin
    (define (p) (list (+ 1 'o)))
    (closure-compile p)))
(test-assertion-violation "subr violation 2" (p))
(test-end)

(test-begin "subr violation 3")
(test-eval!
  (begin
    (define (p) (list (+ 1 'o) (- 2 'p)))
    (closure-compile p)))
(test-assertion-violation "subr violation 3" (p))
(test-end)

(test-begin "subr violation 4")
(test-eval!
  (begin
    (define (p) (list (car 'o) 7))
    (closure-compile p)))
(test-assertion-violation "subr violation 4" (p))
(test-end)

(test-begin "subr violation 5")
(test-eval!
  (begin
    (define (p) (+ (list 1) 7))
    (closure-compile p)))
(test-assertion-violation "subr violation 5" (p))
(test-end)

(test-begin "subr violation 6")
(test-eval!
  (begin
    (define (p) (if (- 'i) 7 8))
    (closure-compile p)))
(test-assertion-violation "subr violation 6" (p))
(test-end)

(test-begin "internal definitions 1")
(test-eval!
    (define p
      (lambda (proc lst1 . lst2)
        (define map-1
          (lambda (proc lst)
            (cond ((null? lst) '())
                  (else (cons (proc (car lst))
                              (map-1 proc (cdr lst)))))))
        (define map-n
          (lambda (proc lst)
            (cond ((null? lst) '())
                  (else (cons (apply proc (car lst))
                              (map-n proc (cdr lst)))))))
        (if (null? lst2)
            (map-1 proc lst1)
            (map-n proc (apply list-transpose* lst1 lst2))))))
(test-eval!
  (closure-compile p))
(test-equal "map-1"
  (p - '(1 2 3)) => (-1 -2 -3))
(test-equal "map-n"
  (p + '(1 2 3) '(1 2 3)) => (2 4 6))
(test-end)

(test-begin "internal definitions 2")
(test-eval!
  (define acc #f))
(test-eval!
  (define add (lambda (n) (set! acc (cons n acc)))))
(test-eval!
    (define (p)
      (define (t v)
        (add v))
      (let loop ((n 0))
        (cond
          ((> n 5) (t #t))
          ((= n 2) (t "*") (loop (+ n 1)))
          (else
            (t n)
            (loop (+ n 1)))))))
(test-eval!
  (closure-compile p))
(test-eval!
  (p))
(test-equal "acc"
  acc => (#t 5 4 3 "*" 1 0 . #f))
(test-end)

(test-begin "parameter")
(test-eval!
  (begin
    (define p (make-parameter 9))
    (closure-compile p)))
(test-equal "before"
  (p) => 9)
(test-eval!
  (p 100))
(test-equal "after"
  (p) => 100)
(test-end)

(test-begin "close")
(test-eval!
  (begin
    (define m)
    (define (n a) (set! m (lambda () (list a 1))))
    (closure-compile n)))
(test-eval!
  (n 100))
(test-equal "eval"
  (m) => (100 1))
(test-end)

(test-begin "ret close")
(test-eval!
  (begin
    (define (m n) (lambda (s) (+ s n)))
    (closure-compile m)))
(test-equal "eval"
  ((m 100) 20) => 120)
(test-end)

;; ./digamma --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/codegen.scm
#|
(backtrace #f)
(define (p n m)
  (let loop1 ((n n))
    (cond ((> n 2))
          (else
            (let loop2 ((m m))
              (cond ((> m 2))
                    (else
                     (display n)(newline)
                     (display m)(newline)
                     (loop2 (+ m 1)))))
            (loop1 (+ n 1))))))
(closure-compile p)
(p 0 0)

- unsupported instruction >.iloc
- unsupported instruction if.true.ret.const

(backtrace #f)
(define (p m)
    (let loop ((n 0))
      (display "n+") (display n) (newline)
      (cond ((> n m) #f)
            (else
              (display "n*") (display n) (newline)
              (loop (+ n 1))))))
(closure-code p)
(closure-compile p)

(backtrace #f)
(import (digamma time))
(define (p m)
    (let loop ((n 0))
      (cond ((> n m) #f)
            (else
              (loop (+ n 1))))))
(closure-code p)
(closure-compile p)
(time (p 1000000))
;;  0.014971 real    0.014894 user    0.000048 sys
|#
