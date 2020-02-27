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
;(time-bench boyer 3)
;(time-bench browse 120)
;(time-bench cpstak 80)
;(time-bench ctak 25)
;(time-bench dderiv 160000)
;(time-bench deriv 320000)
;(time-bench destruc 100)
(time-bench diviter 200000)
(time-bench divrec 140000)
;(time-bench puzzle 12)
(time-bench takl 35)
;(time-bench triangl 1)

(format #t "\n\n;;  ARITHMETIC\n")
;(time-bench fft 200)
;(time-bench fib 1)
;(time-bench fibc 50)
;(time-bench fibfp 1)
;(time-bench mbrot 10)
;(time-bench nucleic 1)
;(time-bench pnpoly 10000)
;(time-bench sum 1000)
;(time-bench sumfp 600)
;(time-bench tak 200)

(format #t "\n\n;;  MISCELLANEOUS\n")
;(time-bench conform 4)
;(time-bench earley 20)
;(time-bench graphs 15)
;(time-bench mazefun 100)
;(time-bench nqueens 150)
;(time-bench paraffins 100)
;(time-bench peval 20)
;(time-bench ray 1)
;(time-bench scheme 3000)

(newline)

; ./digamma --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib -- bench/run-digamma-jit.scm
; ./digamma --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib -- bench/run-digamma.scm

;;  ack     (x3)
;;  2.446550 real    4.692959 user    0.090377 sys
;;  ----------------------------------------------------------------
;;  diviter (x200000)
;;  1.149196 real    1.595736 user    0.011249 sys
;;  ----------------------------------------------------------------
;;  divrec  (x140000)
;;  0.908912 real    1.553600 user    0.018883 sys
;;  ----------------------------------------------------------------
;;  takl    (x35)
;;  0.316298 real    0.316725 user    0.000267 sys
;;  ----------------------------------------------------------------

;;  ack     (x3)
;;  3.363647 real    5.491526 user    0.082901 sys
;;  ----------------------------------------------------------------
;;  diviter (x200000)
;;  1.576578 real    2.021729 user    0.013038 sys
;;  ----------------------------------------------------------------
;;  divrec  (x140000)
;;  1.503382 real    2.154717 user    0.016229 sys
;;  ----------------------------------------------------------------
;;  takl    (x35)
;;  0.831721 real    0.831120 user    0.000440 sys
;;  ----------------------------------------------------------------

;; linux
;;  ack     (x3)
;;  2.052531 real    3.948433 user    0.007817 sys
;;  ----------------------------------------------------------------
;;  diviter (x200000)
;;  0.720344 real    1.322968 user    0.003998 sys
;;  ----------------------------------------------------------------
;;  divrec  (x140000)
;;  0.771966 real    1.396084 user    0.000004 sys
;;  ----------------------------------------------------------------
;;  takl    (x35)
;;  0.248926 real    0.248927 user    0.000001 sys
;;  ----------------------------------------------------------------

;;  ack     (x3)
;;  2.581990 real    4.380421 user    0.047378 sys
;;  ----------------------------------------------------------------
;;  diviter (x200000)
;;  0.954563 real    1.614680 user    0.000000 sys
;;  ----------------------------------------------------------------
;;  divrec  (x140000)
;;  0.942950 real    1.576778 user    0.004133 sys
;;  ----------------------------------------------------------------
;;  takl    (x35)
;;  0.603335 real    0.603332 user    0.000000 sys
;;  ----------------------------------------------------------------
