(define-module (core cffi)
  (export load-shared-object
          lookup-shared-object
          c-function
          c-function/weak
          c-callback)

  (define c-type-class
    `((void               . #\i)
      (bool               . #\b)
      (char               . #\u)
      (short              . #\d)
      (int                . #\q)
      (long               . #\o)
      (long-long          . #\o)
      (unsigned-short     . #\d)
      (unsigned-int       . #\q)
      (unsigned-long      . #\o)
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
      (size_t             . #\o)
      (void*              . #\o)))

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
        ((_ ret name (args ...) (varargs ...))
         (let ((signature1 (make-signature (syntax->datum #'(ret args ... varargs ...))))
               (signature2 (make-signature (syntax->datum #'(ret args ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature1 #,signature2))))))

  (define-syntax c-function/weak
    (syntax-rules ()
      ((_ . args)
       (let ((thunk #f))
         (lambda e
           (cond (thunk (apply thunk e))
                 (else
                   (set! thunk (c-function . args))
                   (apply thunk e))))))))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
        ((_ ret (args ...) closure)
         (let ((signature (make-signature (syntax->datum #'(ret args ...)))))
            #`(codegen-cdecl-callback closure #,signature))))))

  ) ;[end]