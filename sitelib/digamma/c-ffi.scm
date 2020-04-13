#!nobacktrace
;;; Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma c-ffi)
  (export load-shared-object
          c-function
          c-function/weak
          c-callback
          string->utf8/nul
          bytevector-mapping?
          make-bytevector-mapping
          lookup-shared-object
          codegen-cdecl-callout
          codegen-cdecl-callback)

  (import (core)
          (digamma assert)
          (digamma concurrent)
          (only (digamma c-types) sizeof:int sizeof:long sizeof:size_t sizeof:void*))

  (define c-type-class
    `((void               . #\i)
      (bool               . #\b)
      (char               . #\u)
      (short              . #\d)
      (int                . ,(if (= sizeof:int 4) #\q #\o))
      (long               . ,(if (= sizeof:long 4) #\q #\o))
      (long-long          . #\o)
      (unsigned-short     . #\d)
      (unsigned-int       . ,(if (= sizeof:int 4) #\q #\o))
      (unsigned-long      . ,(if (= sizeof:long 4) #\q #\o))
      (unsigned-long-long . #\o)
      (int8_t             . #\u)
      (int16_t            . #\d)
      (int32_t            . #\q)
      (int64_t            . #\o)
      (uint8_t            . #\u)
      (uint16_t           . #\d)
      (uint32_t           . #\q)
      (uint64_t           . #\o)
      (float              . #\s)
      (double             . #\x)
      (size_t             . ,(if (= sizeof:size_t 4) #\q #\o))
      (void*              . ,(if (= sizeof:void* 4) #\q #\o))))

  (define make-signature
    (lambda (types)
      (apply string (map (lambda (type)
             (cond ((assq type c-type-class) => cdr)
                   (else (assertion-violation 'make-signature (format "invalid argument type ~u" type)))))
           types))))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        ((_ ret name (args ...))
         (let ((signature (make-signature (syntax->datum #'(ret args ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature)))
        ((_ ret name (args ...) (proto ...))
         (let ((signature1 (make-signature (syntax->datum #'(ret args ...))))
               (signature2 (make-signature (syntax->datum #'(proto ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature1 #,signature2))))))

  (define-syntax c-function/weak
    (lambda (x)
      (syntax-case x ()
        ((_ . args)
         #'(let ((thunk0 (make-parameter #f)))
             (lambda e
               (let ((thunk (thunk0)))
                 (cond ((and thunk (local-heap-object? thunk)) (apply thunk e))
                       (else
                         (let ((thunk (c-function . args))) (thunk0 thunk) (apply thunk e)))))))))))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
        ((_ ret (args ...) closure)
         (let ((signature (make-signature (syntax->datum #'(ret args ...)))))
            #`(begin (codegen-queue-push! closure) (codegen-cdecl-callback closure #,signature)))))))

  ) ;[end]

#|

(library (digamma qsort)
  (export qsort)
  (import (core) (digamma c-ffi))
  (define qsort (c-function/weak void qsort (void* int int void*))))

(import (digamma c-ffi))
(import (digamma qsort))

(define comparison
  (c-callback int (void* void*)
    (lambda (a1 a2)
      (display "[scheme proc invoked]") (newline)
      (let ((n1 (bytevector-u32-native-ref (make-bytevector-mapping a1 4) 0))
            (n2 (bytevector-u32-native-ref (make-bytevector-mapping a2 4) 0)))
        (cond ((= n1 n2) 0)
              ((< n1 n2) 1)
              (else -1))))))

(define nums (uint-list->bytevector '(10000 1000 10 100000 100) (native-endianness) 4))
(qsort nums 5 4 comparison)
(bytevector->uint-list nums (native-endianness) 4)

;;;

(define printf* (c-function int printf (void* int void* double double) (void*)))
(printf* (string->utf8/nul "%d %s %lf %lf\n") 293 (string->utf8/nul "hello") 1.2 4.5)

(define printf* (c-function/weak int printf (void* int void* double double) (void*)))
(printf* (string->utf8/nul "%d %s %lf %lf\n") 293 (string->utf8/nul "hello") 1.2 4.5)

|#
