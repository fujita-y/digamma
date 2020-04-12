#!nobacktrace
;;; Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma c-ffi)
  (export c-function
          c-function/weak
          c-callback
          load-shared-object
          lookup-shared-object
          define-c-enum
          define-c-typedef
          define-c-struct-type
          define-c-struct-methods
          c-sizeof
          c-coerce-void*
          bytevector-mapping?
          make-bytevector-mapping
          bytevector-c-bool-ref
          bytevector-c-short-ref
          bytevector-c-int-ref
          bytevector-c-long-ref
          bytevector-c-long-long-ref
          bytevector-c-void*-ref
          bytevector-c-float-ref
          bytevector-c-double-ref
          bytevector-c-unsigned-short-ref
          bytevector-c-unsigned-int-ref
          bytevector-c-unsigned-long-ref
          bytevector-c-unsigned-long-long-ref
          bytevector-c-bool-set!
          bytevector-c-short-set!
          bytevector-c-int-set!
          bytevector-c-long-set!
          bytevector-c-long-long-set!
          bytevector-c-void*-set!
          bytevector-c-float-set!
          bytevector-c-double-set!
          bytevector-c-int8-ref
          bytevector-c-int16-ref
          bytevector-c-int32-ref
          bytevector-c-int64-ref
          bytevector-c-uint8-ref
          bytevector-c-uint16-ref
          bytevector-c-uint32-ref
          bytevector-c-uint64-ref
          bytevector-c-int8-set!
          bytevector-c-int16-set!
          bytevector-c-int32-set!
          bytevector-c-int64-set!
          bytevector-c-strlen
          make-c-bool
          make-c-short
          make-c-int
          make-c-long
          make-c-long-long
          make-c-void*
          make-c-float
          make-c-double
          make-c-int8
          make-c-int16
          make-c-int32
          make-c-int64
          make-c-string
          c-bool-ref
          c-short-ref
          c-int-ref
          c-long-ref
          c-long-long-ref
          c-void*-ref
          c-float-ref
          c-double-ref
          c-unsigned-short-ref
          c-unsigned-int-ref
          c-unsigned-long-ref
          c-unsigned-long-long-ref
          c-int8-ref
          c-int16-ref
          c-int32-ref
          c-int64-ref
          c-uint8-ref
          c-uint16-ref
          c-uint32-ref
          c-uint64-ref
          c-string-ref
          c-bool-set!
          c-short-set!
          c-int-set!
          c-long-set!
          c-void*-set!
          c-float-set!
          c-double-set!
          c-int8-set!
          c-int16-set!
          c-int32-set!
          c-int64-set!
          c-string-set!
          sizeof:bool
          sizeof:short
          sizeof:int
          sizeof:long
          sizeof:long-long
          sizeof:void*
          sizeof:size_t
          alignof:bool
          alignof:short
          alignof:int
          alignof:long
          alignof:long-long
          alignof:void*
          alignof:size_t
          alignof:float
          alignof:double
          alignof:int8_t
          alignof:int16_t
          alignof:int32_t
          alignof:int64_t)

  (import (core) (digamma concurrent) (digamma c-types) (digamma assert))

  (define exact-integer?
    (lambda (i)
      (and (integer? i) (exact? i))))

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

  (define make-signatures
    (lambda (name types)
      (apply string (map (lambda (type)
             (cond ((assq type c-type-class) => cdr)
                   (else (assertion-violation name (format "invalid argument type ~u" type)))))
           types))))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        ((_ ret name (args ...))
         (let ((signature (make-signatures (syntax->datum #'name) (syntax->datum #'(ret args ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature)))
        ((_ ret name (args ...) (proto ...))
         (let ((signature1 (make-signatures (syntax->datum #'name) (syntax->datum #'(ret args ...))))
               (signature2 (make-signatures (syntax->datum #'name) (syntax->datum #'(proto ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature1 #,signature2))))))

  (define-syntax c-function/weak
    (lambda (x)
      (syntax-case x ()
        ((_ ret name (args ...))
         (let ((signature
                 (make-signatures
                   (syntax->datum #'name)
                   (syntax->datum #'(ret args ...)))))
           #`(letrec ((thunk
                        (lambda e
                          (set! thunk
                                (codegen-cdecl-callout
                                  (lookup-shared-object 'name)
                                  #,signature))
                          (apply thunk e))))
               (lambda e (apply thunk e)))))
        ((_ ret name (args ...) (proto ...))
         (let ((signature1
                 (make-signatures
                   (syntax->datum #'name)
                   (syntax->datum #'(ret args ...))))
               (signature2
                 (make-signatures
                   (syntax->datum #'name)
                   (syntax->datum #'(proto ...)))))
           #`(letrec ((thunk
                        (lambda e
                          (set! thunk
                                (codegen-cdecl-callout
                                  (lookup-shared-object 'name)
                                  #,signature1
                                  #,signature2))
                          (apply thunk e))))
               (lambda e (apply thunk e))))))))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
        ((_ ret (args ...) closure)
         (let ((signature (make-signatures (syntax->datum #'name) (syntax->datum #'(ret args ...)))))
            #`(begin (codegen-queue-push! closure) (codegen-cdecl-callback closure #,signature)))))))

  ) ;[end]

#|
(library (digamma math)
  (export qsort)
  (import (core) (digamma c-ffi))
  (define qsort (c-function/weak void qsort (void* int int void*))))

(import (digamma c-ffi))
(import (digamma math))

(define comparison
  (c-callback int (void* void*)
    (lambda (a1 a2)
      (display "[scheme proc invoked]") (newline)
      (let ((n1 (bytevector-u32-native-ref (make-bytevector-mapping a1 4) 0))
            (n2 (bytevector-u32-native-ref (make-bytevector-mapping a2 4) 0)))
        (cond ((= n1 n2) 0)
              ((> n1 n2) 1)
              (else -1))))))

(define nums (uint-list->bytevector '(10000 1000 10 100000 100) (native-endianness) 4))
(qsort nums 5 4 comparison)

(define printf* (c-function int printf (void* int void* double double) (void*)))
(printf* (string->utf8/nul "%d %s %lf %lf\n") 293 (string->utf8/nul "hello") 1.2 4.5)

(define printf* (c-function/weak int printf (void* int void* double double) (void*)))
(printf* (string->utf8/nul "%d %s %lf %lf\n") 293 (string->utf8/nul "hello") 1.2 4.5)

|#
