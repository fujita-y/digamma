#!/usr/bin/env digamma

(import (core))

(add-load-path "./gambit-benchmarks")
(add-load-path "./bench/gambit-benchmarks")

(define-syntax time
  (syntax-rules ()
    ((_ expr)
     (destructuring-bind (real-start user-start sys-start) (time-usage)
       (let ((result (apply (lambda () expr) '())))
         (destructuring-bind (real-end user-end sys-end) (time-usage)
           (format #t
                   "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
                   (- real-end real-start)
                   (- user-end user-start)
                   (- sys-end sys-start)))
         result)))))

(define (run-benchmark name count ok? run-maker . args)
  (format #t "~%;;  ~a (x~a)~!" (pad-space name 7) count)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (and (not (ok? result)) (format #t "~%;; wrong result: ~s~%~!" result)))
  (format #t ";;  ----------------------------------------------------------------~!")
  (unspecified))

(define call-with-output-file/truncate
  (lambda (file-name proc)
    (let ((p (open-file-output-port
              file-name
              (file-options no-fail)
              (buffer-mode block)
              (native-transcoder))))
      (call-with-port p proc))))

(define fatal-error
  (lambda x
    (format #t "fatal-error: ~s" x)
    (exit)))

(define pad-space
  (lambda (str n)
    (let ((pad (- n (string-length str))))
      (if (<= pad 0)
          str
          (string-append str (make-string pad #\space))))))

(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
        (loop (+ i 1) (run))
        result)))

(define load-bench-n-run
  (lambda (name)
    (load (string-append name ".scm"))
    (compile)
    (main)))

(define-syntax time-bench
  (lambda (x)
    (syntax-case x ()
      ((?_ name count)
       (let ((symbolic-name (syntax->datum (syntax name))))
         (with-syntax ((symbol-iters (datum->syntax #'?_ (string->symbol (format "~a-iters" symbolic-name))))
                       (string-name (datum->syntax #'?_ (symbol->string symbolic-name))))
           (syntax
            (begin
              (define symbol-iters count)
              (load-bench-n-run string-name)))))))))

(define-syntax FLOATvector-const (syntax-rules () ((_ . lst) (list->vector 'lst))))
(define-syntax FLOATvector? (syntax-rules () ((_ x) (vector? x))))
(define-syntax FLOATvector (syntax-rules () ((_ . lst) (vector . lst))))
(define-syntax FLOATmake-vector (syntax-rules () ((_ n . init) (make-vector n . init))))
(define-syntax FLOATvector-ref (syntax-rules () ((_ v i) (vector-ref v i))))
(define-syntax FLOATvector-set! (syntax-rules () ((_ v i x) (vector-set! v i x))))
(define-syntax FLOATvector-length (syntax-rules () ((_ v) (vector-length v))))
(define-syntax nuc-const (syntax-rules () ((_ . lst) (list->vector 'lst))))
(define-syntax FLOAT+ (syntax-rules () ((_ . lst) (+ . lst))))
(define-syntax FLOAT- (syntax-rules () ((_ . lst) (- . lst))))
(define-syntax FLOAT* (syntax-rules () ((_ . lst) (* . lst))))
(define-syntax FLOAT/ (syntax-rules () ((_ . lst) (/ . lst))))
(define-syntax FLOAT= (syntax-rules () ((_ . lst) (= . lst))))
(define-syntax FLOAT< (syntax-rules () ((_ . lst) (< . lst))))
(define-syntax FLOAT<= (syntax-rules () ((_ . lst) (<= . lst))))
(define-syntax FLOAT> (syntax-rules () ((_ . lst) (> . lst))))
(define-syntax FLOAT>= (syntax-rules () ((_ . lst) (>= . lst))))
(define-syntax FLOATnegative? (syntax-rules () ((_ x) (negative? x))))
(define-syntax FLOATpositive? (syntax-rules () ((_ x) (positive? x))))
(define-syntax FLOATzero? (syntax-rules () ((_ x) (zero? x))))
(define-syntax FLOATabs (syntax-rules () ((_ x) (abs x))))
(define-syntax FLOATsin (syntax-rules () ((_ x) (sin x))))
(define-syntax FLOATcos (syntax-rules () ((_ x) (cos x))))
(define-syntax FLOATatan (syntax-rules () ((_ x) (atan x))))
(define-syntax FLOATsqrt (syntax-rules () ((_ x) (sqrt x))))
(define-syntax FLOATmin (syntax-rules () ((_ . lst) (min . lst))))
(define-syntax FLOATmax (syntax-rules () ((_ . lst) (max . lst))))
(define-syntax FLOATround (syntax-rules () ((_ x) (round x))))
(define-syntax FLOATinexact->exact (syntax-rules () ((_ x) (inexact->exact x))))
(define-syntax GENERIC+ (syntax-rules () ((_ . lst) (+ . lst))))
(define-syntax GENERIC- (syntax-rules () ((_ . lst) (- . lst))))
(define-syntax GENERIC* (syntax-rules () ((_ . lst) (* . lst))))
(define-syntax GENERIC/ (syntax-rules () ((_ . lst) (/ . lst))))
(define-syntax GENERICquotient (syntax-rules () ((_ x y) (quotient x y))))
(define-syntax GENERICremainder (syntax-rules () ((_ x y) (remainder x y))))
(define-syntax GENERICmodulo (syntax-rules () ((_ x y) (modulo x y))))
(define-syntax GENERIC= (syntax-rules () ((_ . lst) (= . lst))))
(define-syntax GENERIC< (syntax-rules () ((_ . lst) (< . lst))))
(define-syntax GENERIC<= (syntax-rules () ((_ . lst) (<= . lst))))
(define-syntax GENERIC> (syntax-rules () ((_ . lst) (> . lst))))
(define-syntax GENERIC>= (syntax-rules () ((_ . lst) (>= . lst))))
(define-syntax GENERICexpt (syntax-rules () ((_ x y) (expt x y))))

#!compatible

(closure-compile run-bench)
;(closure-compile run-benchmark)

(format #t "\n\n;;  GABRIEL\n")
(time-bench ack 3)
(time-bench boyer 3)
(time-bench browse 120)
(time-bench cpstak 80)
(time-bench ctak 25)
(time-bench dderiv 160000)
(time-bench deriv 320000)
(time-bench destruc 100)
(time-bench diviter 200000)
(time-bench divrec 140000)
(time-bench puzzle 12)
(time-bench takl 35)
(time-bench triangl 1)

(format #t "\n\n;;  ARITHMETIC\n")
(time-bench fft 200)
(time-bench fib 1)
(time-bench fibc 50)
(time-bench fibfp 1)
(time-bench mbrot 10)
;(time-bench nucleic 1)
(time-bench pnpoly 10000)
(time-bench sum 1000)
(time-bench sumfp 600)
(time-bench tak 200)

(format #t "\n\n;;  MISCELLANEOUS\n")
(time-bench conform 4)
;(time-bench earley 20)
;(time-bench graphs 15)
;(time-bench mazefun 100)
;(time-bench nqueens 150)
(time-bench paraffins 100)
;(time-bench peval 20)
;(time-bench ray 1)
(time-bench scheme 3000)

(newline)
(exit)
; ./digamma --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib -- bench/run-digamma-jit.scm
; ./digamma --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib -- bench/run-digamma.scm

;;  ack     (x3)
;;  2.470866 real    4.710954 user    0.089389 sys
;;  ----------------------------------------------------------------
;;  boyer   (x3)
;;  0.424072 real    0.442156 user    0.000884 sys
;;  ----------------------------------------------------------------
;;  browse  (x120)
;;  0.379149 real    0.408624 user    0.001122 sys
;;  ----------------------------------------------------------------
;;  cpstak  (x80)
;;  0.724463 real    1.124489 user    0.012644 sys
;;  ----------------------------------------------------------------
;;  ctak    (x25)
;;  0.337657 real    0.655112 user    0.009606 sys
;;  ----------------------------------------------------------------
;;  dderiv  (x160000)
;;  0.744946 real    0.909694 user    0.004459 sys
;;  ----------------------------------------------------------------
;;  diviter (x200000)
;;  0.697188 real    1.230245 user    0.015284 sys
;;  ----------------------------------------------------------------
;;  divrec  (x140000)
;;  0.959311 real    1.637071 user    0.016073 sys
;;  ----------------------------------------------------------------
;;  takl    (x35)
;;  0.356460 real    0.355921 user    0.000863 sys
;;  ----------------------------------------------------------------
;;  fib     (x1)
;;  0.870725 real    0.869811 user    0.000574 sys
;;  ----------------------------------------------------------------


;;  dderiv  (x160000)
;;  0.785017 real    0.939485 user    0.004217 sys
;;  ----------------------------------------------------------------generating native code: create-n

(level - 1) / 2

(backtrace #f)
(define (p n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
      (let loop2 ((x (- n 1)))
        (if (>= x 0)
            (loop2 (- x 1))
            (loop1 (- y 1)))))))

((extend.enclose+
   (>=n.iloc (0 . 0) 0)
   (if.true
     (extend.enclose+
       (>=n.iloc (0 . 0) 0)
       (if.true
         (push.n+.iloc (0 . 0) -1) x -1
         (apply.iloc+ (1 . 0))) loop 2
       (push.n+.iloc (2 . 0) -1)
       (apply.iloc+ (3 . 0))) loop 1
     (push.n+.iloc (3 . 0) -1)
     (apply.iloc+ (0 . 0)))
   (ret.const . #<unspecified>))
 (push.n+.iloc (1 . 0) -1)
 (apply.iloc+ (0 . 0)))

 (backtrace #f)
(define (p n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
      (let loop2 ((x (- n 1)))
            (loop2 (- x 1))
            (loop1 (- y 1))))))

((extend.enclose+
   (>=n.iloc (0 . 0) 0)
   (if.true
    (extend.enclose . #<closure loop2>)
    (push.n+.iloc (3 . 0) -1)
    (apply.iloc (0 . 0)))
   (ret.const . #<unspecified>))
 (push.n+.iloc (1 . 0) -1)
 (apply.iloc+ (0 . 0)))


====

(backtrace #f)
(define (p n)
  (let loop0 ((y (- n 1)))
    (if (>= y 0)
      (let loop1 ((y (- n 1)))
        (if (>= y 0)
          (let loop2 ((x (- n 1)))
            (if (>= x 0)
                (loop0 (- x 1)))))))))
(closure-code p)

((extend.enclose+
   (>=n.iloc (0 . 0) 0)
   (if.true
     (extend.enclose+
       (>=n.iloc (0 . 0) 0)
       (if.true
         (extend.enclose+
           (>=n.iloc (0 . 0) 0)
           (if.true
              (push.n+.iloc (0 . 0) -1)
              (apply.iloc+ (5 . 0)))
           (ret.const . #<unspecified>))
         (push.n+.iloc (5 . 0) -1)
         (apply.iloc+ (0 . 0)))
       (ret.const . #<unspecified>))
     (push.n+.iloc (3 . 0) -1)
     (apply.iloc+ (0 . 0)))
   (ret.const . #<unspecified>))
 (push.n+.iloc (1 . 0) -1)
 (apply.iloc+ (0 . 0)))

=====
(backtrace #f)
(define (p n)
  (let loop0 ((y (- n 1)))
    (if (>= y 0)
      (let loop1 ((y (- n 1)))
        (if (>= y 0)
          (let loop2 ((x (- n 1)))
            (if (>= x 0)
                (loop1 (- x 1)))))))))
(closure-code p)

((extend.enclose+
   (>=n.iloc (0 . 0) 0)
   (if.true
     (extend.enclose+
       (>=n.iloc (0 . 0) 0)
       (if.true
         (extend.enclose+
           (>=n.iloc (0 . 0) 0)
           (if.true (push.n+.iloc (0 . 0) -1) (apply.iloc+ (3 . 0)))
           (ret.const . #<unspecified>))
         (push.n+.iloc (5 . 0) -1)
         (apply.iloc+ (0 . 0)))
       (ret.const . #<unspecified>))
     (push.n+.iloc (3 . 0) -1)
     (apply.iloc+ (0 . 0)))
   (ret.const . #<unspecified>))
 (push.n+.iloc (1 . 0) -1)
 (apply.iloc+ (0 . 0)))


(backtrace #f)
(define (p n)
  (let loop0 ((y (- n 1)))
    (if (>= y 0)
      (let loop1 ((y (- n 1)))
        (if (>= y 0)
          (let loop2 ((x (- n 1)))
            (if (>= x 0)
                (loop2 (- x 1)))))))))
(closure-code p)

vector at 0 1 2
depth     0 1 2
index     5 3 1

2 5 -> 0
2 3 -> 1
2 1 -> 2

(level - 1) / 2

====

(backtrace #f)
(define (p n)
  (let loop0 ((y (- n 1)))
    (if (>= y 0)
      (let loop1 ()
        (if (>= y 0)
          (let loop2 ((x (- n 1)))
            (if (>= x 0)
                (loop0 (- x 1))
                (loop2 (- x 1)))))))))
(closure-code p)
