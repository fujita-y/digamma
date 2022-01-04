#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma c-types)
  (export define-c-enum
          define-c-typedef
          define-c-struct-methods
          c-sizeof
          c-coerce-void*
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
          c-long-long-set!
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

  (import (core))

  (define sizeof:bool       (architecture-feature 'sizeof:bool))
  (define sizeof:int        (architecture-feature 'sizeof:int))
  (define sizeof:long       (architecture-feature 'sizeof:long))
  (define sizeof:long-long  (architecture-feature 'sizeof:long-long))
  (define sizeof:short      (architecture-feature 'sizeof:short))
  (define sizeof:void*      (architecture-feature 'sizeof:void*))
  (define sizeof:size_t     (architecture-feature 'sizeof:size_t))
  (define alignof:bool      (architecture-feature 'alignof:bool))
  (define alignof:int       (architecture-feature 'alignof:int))
  (define alignof:long      (architecture-feature 'alignof:long))
  (define alignof:long-long (architecture-feature 'alignof:long-long))
  (define alignof:short     (architecture-feature 'alignof:short))
  (define alignof:void*     (architecture-feature 'alignof:void*))
  (define alignof:size_t    (architecture-feature 'alignof:size_t))
  (define alignof:float     (architecture-feature 'alignof:float))
  (define alignof:double    (architecture-feature 'alignof:double))
  (define alignof:int8_t    (architecture-feature 'alignof:int8_t))
  (define alignof:int16_t   (architecture-feature 'alignof:int16_t))
  (define alignof:int32_t   (architecture-feature 'alignof:int32_t))
  (define alignof:int64_t   (architecture-feature 'alignof:int64_t))

  (define coerce-bool
    (lambda (x)
      (cond ((eq? x #t) 1)
            ((eq? x #f) 0)
            ((and (integer? x) (exact? x))
             (if (= x 0) 0 1))
            (else
             (assertion-violation 'bytevector-c-bool-set! (format "expected boolean or exact integer, but got ~r, as argument 3" x))))))

  (define bytevector-c-bool-ref
    (case sizeof:bool
      ((1) (lambda (bv offset) (if (> (bytevector-c-uint8-ref  bv offset) 0) 1 0)))
      ((4) (lambda (bv offset) (if (> (bytevector-c-uint32-ref bv offset) 0) 1 0)))
      ((8) (lambda (bv offset) (if (> (bytevector-c-uint64-ref bv offset) 0) 1 0)))
      (else (assertion-violation 'bytevector-c-bool-ref "internal inconsistency"))))

  (define bytevector-c-bool-set!
    (case sizeof:bool
      ((1) (lambda (bv offset value) (bytevector-c-int8-set!  bv offset (coerce-bool value))))
      ((4) (lambda (bv offset value) (bytevector-c-int32-set! bv offset (coerce-bool value))))
      ((8) (lambda (bv offset value) (bytevector-c-int64-set! bv offset (coerce-bool value))))
      (else (assertion-violation 'bytevector-c-bool-set! "internal inconsistency"))))

  (define primitive-syntax
    (let ((ht (make-eq-hashtable)))
      (for-each
        (lambda (b) (hashtable-set! ht (car b) (cdr b)))
        `((bytevector-c-int-ref            . ,#'bytevector-c-int-ref)
          (bytevector-c-bool-ref           . ,#'bytevector-c-bool-ref)
          (bytevector-c-long-ref           . ,#'bytevector-c-long-ref)
          (bytevector-c-short-ref          . ,#'bytevector-c-short-ref)
          (bytevector-c-void*-ref          . ,#'bytevector-c-void*-ref)
          (bytevector-c-unsigned-int-ref   . ,#'bytevector-c-unsigned-int-ref)
          (bytevector-c-unsigned-long-ref  . ,#'bytevector-c-unsigned-long-ref)
          (bytevector-c-unsigned-short-ref . ,#'bytevector-c-unsigned-short-ref)
          (bytevector-c-float-ref          . ,#'bytevector-c-float-ref)
          (bytevector-c-double-ref         . ,#'bytevector-c-double-ref)
          (bytevector-c-int8-ref           . ,#'bytevector-c-int8-ref)
          (bytevector-c-int16-ref          . ,#'bytevector-c-int16-ref)
          (bytevector-c-int32-ref          . ,#'bytevector-c-int32-ref)
          (bytevector-c-int64-ref          . ,#'bytevector-c-int64-ref)
          (bytevector-c-uint8-ref          . ,#'bytevector-c-uint8-ref)
          (bytevector-c-uint16-ref         . ,#'bytevector-c-uint16-ref)
          (bytevector-c-uint32-ref         . ,#'bytevector-c-uint32-ref)
          (bytevector-c-uint64-ref         . ,#'bytevector-c-uint64-ref)
          (bytevector-c-bool-set!          . ,#'bytevector-c-bool-set!)
          (bytevector-c-int-set!           . ,#'bytevector-c-int-set!)
          (bytevector-c-long-set!          . ,#'bytevector-c-long-set!)
          (bytevector-c-short-set!         . ,#'bytevector-c-short-set!)
          (bytevector-c-void*-set!         . ,#'bytevector-c-void*-set!)
          (bytevector-c-int8-set!          . ,#'bytevector-c-int8-set!)
          (bytevector-c-int16-set!         . ,#'bytevector-c-int16-set!)
          (bytevector-c-int32-set!         . ,#'bytevector-c-int32-set!)
          (bytevector-c-int64-set!         . ,#'bytevector-c-int64-set!)
          (bytevector-c-float-set!         . ,#'bytevector-c-float-set!)
          (bytevector-c-double-set!        . ,#'bytevector-c-double-set!)
          (make-bytevector                 . ,#'make-bytevector)
          (bytevector-copy!                . ,#'bytevector-copy!)))
      (lambda (id)
        (or (hashtable-ref ht id #f)
            (assertion-violation 'primitive-syntax "internal inconsistency")))))

  (define primitive-types
    (let ((ht (make-eq-hashtable)))
      (for-each
       (lambda (desc)
         (destructuring-bind (type sizeof alignof accessor mutator) desc
           (hashtable-set! ht type (list type sizeof alignof (list 'primitive accessor mutator)))))
       `((char               1                 1                  bytevector-c-uint8-ref              bytevector-c-int8-set!)
         (bool               ,sizeof:bool      ,alignof:bool      bytevector-c-bool-ref               bytevector-c-bool-set!)
         (short              ,sizeof:short     ,alignof:short     bytevector-c-short-ref              bytevector-c-short-set!)
         (int                ,sizeof:int       ,alignof:int       bytevector-c-int-ref                bytevector-c-int-set!)
         (long               ,sizeof:long      ,alignof:long      bytevector-c-long-ref               bytevector-c-long-set!)
         (long-long          ,sizeof:long-long ,alignof:long-long bytevector-c-long-long-ref          bytevector-c-long-long-set!)
         (char*              ,sizeof:void*     ,alignof:void*     bytevector-c-void*-ref              bytevector-c-void*-set!)
         (void*              ,sizeof:void*     ,alignof:void*     bytevector-c-void*-ref              bytevector-c-void*-set!)
         (unsigned-short     ,sizeof:short     ,alignof:short     bytevector-c-unsigned-short-ref     bytevector-c-short-set!)
         (unsigned-int       ,sizeof:int       ,alignof:int       bytevector-c-unsigned-int-ref       bytevector-c-int-set!)
         (unsigned-long      ,sizeof:long      ,alignof:long      bytevector-c-unsigned-long-ref      bytevector-c-long-set!)
         (unsigned-long-long ,sizeof:long-long ,alignof:long-long bytevector-c-unsigned-long-long-ref bytevector-c-long-long-set!)
         (float              4                 ,alignof:float     bytevector-c-float-ref              bytevector-c-float-set!)
         (double             8                 ,alignof:double    bytevector-c-double-ref             bytevector-c-double-set!)
         (int8_t             1                 ,alignof:int8_t    bytevector-c-int8-ref               bytevector-c-int8-set!)
         (int16_t            2                 ,alignof:int16_t   bytevector-c-int16-ref              bytevector-c-int16-set!)
         (int32_t            4                 ,alignof:int32_t   bytevector-c-int32-ref              bytevector-c-int32-set!)
         (int64_t            8                 ,alignof:int64_t   bytevector-c-int64-ref              bytevector-c-int64-set!)
         (uint8_t            1                 ,alignof:int8_t    bytevector-c-uint8-ref              bytevector-c-int8-set!)
         (uint16_t           2                 ,alignof:int16_t   bytevector-c-uint16-ref             bytevector-c-int16-set!)
         (uint32_t           4                 ,alignof:int32_t   bytevector-c-uint32-ref             bytevector-c-int32-set!)
         (uint64_t           8                 ,alignof:int64_t   bytevector-c-uint64-ref             bytevector-c-int64-set!)
         (size_t             ,sizeof:size_t
                             ,alignof:size_t
                             ,@(cond ((= sizeof:size_t sizeof:int)
                                      '(bytevector-c-unsigned-int-ref bytevector-c-int-set!))
                                     ((= sizeof:size_t sizeof:long)
                                      '(bytevector-c-unsigned-long-ref bytevector-c-long-set!))
                                     (else
                                      (assertion-violation 'primitive-types "internal inconsistency"))))))
      (hashtable-copy ht)))

  (define constructor-name
    (lambda (stx struct-name)
      (datum->syntax stx (string->symbol (format "make-~a" struct-name)))))

  (define accessor-name
    (lambda (stx struct-name field-name)
      (datum->syntax stx (string->symbol (format "~a-~a" struct-name field-name)))))

  (define mutator-name
    (lambda (stx struct-name field-name)
      (datum->syntax stx (string->symbol (format "~a-~a-set!" struct-name field-name)))))

  (define make-constructor
    (lambda (stx struct-name struct-size)
      #`(begin
          (define #,(constructor-name stx struct-name)
            (lambda ()
              (make-bytevector #,struct-size))))))

  (define make-accessor/mutator
    (lambda (stx struct-name field-name index accessor mutator)
      (let ((accessor (primitive-syntax accessor)) (mutator (primitive-syntax mutator)))
        #`(begin
            (define-syntax #,(accessor-name stx struct-name field-name)
              (syntax-rules ()
                ((_ bv)
                 (#,accessor bv #,index))))
            (define-syntax #,(mutator-name stx struct-name field-name)
              (syntax-rules ()
                ((_ bv obj)
                 (#,mutator bv #,index obj))))))))

  (define make-compound-accessor/mutator
    (lambda (stx struct-name field-name index size make copy!)
      (let ((make (primitive-syntax make)) (copy! (primitive-syntax copy!)))
        #`(begin
            (define-syntax #,(accessor-name stx struct-name field-name)
              (syntax-rules ()
                ((_ bv)
                 (let ((value (#,make #,size)))
                   (begin (#,copy! bv #,index value 0 #,size) value)))))
            (define-syntax #,(mutator-name stx struct-name field-name)
              (syntax-rules ()
                ((_ bv obj)
                 (#,copy! obj 0 bv #,index #,size))))))))

  (define process-struct-fields
    (lambda (code? who stx struct-name compound-types field-specs)

      (define align-field-offset
        (lambda (offset align)
          (bitwise-and (+ offset (- align 1)) (bitwise-not (- align 1)))))

      (let loop ((specs field-specs) (field-offset 0) (struct-align 0) (field-defs '()))
        (if (pair? specs)
            (destructuring-match (car specs)
              ((field-type field-name)
               (let ((desc (or (hashtable-ref primitive-types field-type #f) (assq field-type compound-types))))
                 (destructuring-match desc
                   ((_ sizeof alignof ('primitive accessor mutator))
                    (let ((index (align-field-offset field-offset alignof)))
                      (loop (cdr specs)
                            (+ index sizeof)
                            (max struct-align alignof)
                            (and code?
                                 (if (eq? field-name '_)
                                     field-defs
                                     (cons (make-accessor/mutator stx struct-name field-name index accessor mutator)
                                           field-defs))))))
                   ((_ sizeof alignof ('struct . _))
                    (let ((index (align-field-offset field-offset alignof)))
                      (loop (cdr specs)
                            (+ index sizeof)
                            (max struct-align alignof)
                            (and code?
                                 (if (eq? field-name '_)
                                     field-defs
                                     (cons (make-compound-accessor/mutator stx struct-name field-name index sizeof 'make-bytevector 'bytevector-copy!)
                                           field-defs))))))
                   (_
                    (assertion-violation who "internal inconsistency")))))
              ((field-type field-name [count])
               (cond ((or (hashtable-ref primitive-types field-type #f) (assq field-type compound-types))
                      => (lambda (rec)
                           (destructuring-bind (_ sizeof alignof . _) rec
                             (let ((index (align-field-offset field-offset alignof)))
                               (loop (cdr specs)
                                     (+ index (* sizeof count))
                                     (max struct-align alignof)
                                     (and code?
                                          (if (eq? field-name '_)
                                              field-defs
                                              (cons (make-compound-accessor/mutator stx struct-name field-name index (* sizeof count) 'make-bytevector 'bytevector-copy!)
                                                    field-defs))))))))
                     (else
                      (assertion-violation who "internal inconsistency"))))
              (_
               (assertion-violation who "internal inconsistency")))

            (list (align-field-offset field-offset struct-align) struct-align field-defs)))))

  (define find-maybe-compound
    (lambda (form field-specs)
      (let loop ((specs field-specs) (compounds '()))
        (define maybe-compound-type
          (lambda (type-name)
            (if (hashtable-ref primitive-types (syntax->datum type-name) #f)
                (loop (cdr specs) compounds)
                (loop (cdr specs) (cons type-name compounds)))))
        (if (null? specs)
            (and (pair? compounds) compounds)
            (syntax-case (car specs) ()
              ((type-name field-name)
               (and (identifier? #'type-name) (identifier? #'field-name))
               (maybe-compound-type #'type-name))
              ((type-name field-name [n])
               (and (identifier? #'type-name) (identifier? #'field-name)
                    (let ((n (syntax->datum #'n)))
                      (and (integer? n) (exact? n) (> n 0))))
               (maybe-compound-type #'type-name))
              (_
               (syntax-violation (car form) "invalid syntax" form)))))))

  (define ensure-c-struct
    (lambda (x id)
      (if (eq? (tuple-ref x 0) 'type:c-typedef)
          (cdr (tuple->list x))
          (syntax-violation 'define-c-struct-methods (format "invalid c-typedef object ~r" id) #f))))

  (define ensure-c-typedef
    (lambda (x id)
      (if (eq? (tuple-ref x 0) 'type:c-typedef)
          (cdr (tuple->list x))
          (syntax-violation 'define-c-typedef (format "invalid c-typedef object ~r" id) #f))))

  (define c-struct-expand
    (lambda (struct-name compound-types field-specs)
      (destructuring-bind (struct-size struct-align field-defs)
          (process-struct-fields #t 'define-c-struct-methods struct-name (syntax->datum struct-name) compound-types field-specs)
        #`(begin
            #,(make-constructor struct-name (syntax->datum struct-name) struct-size)
            #,@field-defs))))

  (define c-typedef-struct-expand
    (lambda (type-name compound-types field-specs)
      (destructuring-bind (struct-size struct-align field-defs)
          (process-struct-fields #f 'define-c-typedef type-name (syntax->datum type-name) compound-types field-specs)
        #`(begin
            (define-syntax #,type-name
              (lambda (x)
                (syntax-case x (#,type-name)
                  (#,type-name #'(tuple 'type:c-typedef '#,type-name #,struct-size #,struct-align (cons 'struct '#,field-specs))))))))))

  (define-syntax define-c-typedef
    (lambda (x)
      (syntax-case x (struct)
        ((_ lhs rhs)
         (and (identifier? #'lhs) (identifier? #'rhs))
         (cond ((hashtable-contains? primitive-types (datum lhs))
                (syntax-violation 'define-c-typedef "attempt to modify primitive type" x))
               ((hashtable-ref primitive-types (datum rhs) #f)
                => (lambda (lst)
                     (with-syntax (((_ sizeof alignof (_ accessor mutator)) (datum->syntax #'k lst)))
                       #'(define-syntax lhs
                           (lambda (x)
                             (syntax-case x (lhs)
                               (lhs #'(tuple 'type:c-typedef 'lhs sizeof alignof (list 'primitive 'accessor 'mutator)))))))))
               (else
                #`(let-syntax
                    ((check-c-typedef
                      (lambda (x)
                        (syntax-case x ()
                          ((_ temp _)
                           (let* ((spec (#,#'ensure-c-typedef rhs 'rhs)))
                             #`(define-syntax temp
                                 (lambda (x)
                                   (syntax-case x (temp)
                                     (temp #'(apply tuple 'type:c-typedef 'lhs '#,(datum->syntax #'k (cdr spec)))))))))))))
                    (check-c-typedef lhs rhs)))))
        ((_ lhs (struct field-specs ...))
         (and (identifier? #'lhs) (pair? #'(field-specs ...)))
         (cond ((hashtable-contains? primitive-types (datum lhs))
                (syntax-violation 'define-c-typedef "attempt to modify primitive type" x))
               ((find-maybe-compound x #'(field-specs ...))
                => (lambda (lst)
                     (with-syntax (((compounds ...) lst))
                       #`(let-syntax
                           ((check-c-struct-fields
                             (lambda (x)
                               (syntax-case x ()
                                 ((_ temp . _)
                                  (with-syntax
                                      (((compound-types (... ...))
                                        (datum->syntax #'k (list (#,#'ensure-c-typedef compounds 'compounds) ...))))
                                    (c-typedef-struct-expand #'temp (datum (compound-types (... ...))) '(field-specs ...))))))))
                           (check-c-struct-fields lhs field-specs ...)))))
               (else
                (c-typedef-struct-expand #'lhs '() (datum (field-specs ...)))))))))

  (define-syntax c-struct-methods-2
    (lambda (x)
      (syntax-case x ()
        ((_ lhs field-specs ...)
         (and (identifier? #'lhs) (pair? #'(field-specs ...)))
         (cond ((find-maybe-compound x #'(field-specs ...))
                => (lambda (lst)
                     (with-syntax (((compounds ...) lst))
                       #`(let-syntax
                           ((check-c-struct-fields
                             (lambda (x)
                               (syntax-case x ()
                                 ((_ temp . _)
                                  (with-syntax
                                      (((compound-types (... ...))
                                        (datum->syntax #'k (list (#,#'ensure-c-struct compounds 'compounds) ...))))
                                    (c-struct-expand #'temp (datum (compound-types (... ...))) '(field-specs ...))))))))
                           (check-c-struct-fields lhs field-specs ...)))))
               (else
                (c-struct-expand #'lhs '() (datum (field-specs ...)))))))))

  (define-syntax c-struct-methods-1
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (and (identifier? #'type))
         (if (hashtable-contains? primitive-types (datum type))
             (syntax-violation 'define-c-struct-methods "expected struct type, but got primitive type" x)
             (with-syntax ((form (datum->syntax #'k x)))
               #`(let-syntax
                   ((check-c-struct
                     (lambda (x)
                       (syntax-case x ()
                         ((_ temp)
                          (let* ((spec (#,#'ensure-c-struct type 'type)))
                            (destructuring-match spec
                              ((_ _ _ ('struct . field-specs))
                               (with-syntax (((field-specs (... ...)) (datum->syntax #'temp field-specs)))
                                 #'(c-struct-methods-2 temp field-specs (... ...))))
                              (_
                               (syntax-violation 'define-c-struct-methods "expected struct type, but got primitive type" 'form)))))))))
                   (check-c-struct type))))))))

  (define-syntax define-c-struct-methods
    (syntax-rules ()
      ((_ type ...)
       (begin
         (c-struct-methods-1 type) ...))))

  (define-syntax c-sizeof
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (cond ((hashtable-ref primitive-types (datum type) #f) => cadr)
               (else
                #'(let-syntax
                    ((c-sizeof
                      (lambda (x)
                        (if (eq? (tuple-ref type 0) 'type:c-typedef)
                            (tuple-ref type 2)
                            (syntax-violation 'c-sizeof (format "expected primitive type or c-typedef object, but got ~s" type) x)))))
                    (c-sizeof type))))))))

  (define-syntax c-coerce-void*
    (syntax-rules ()
      ((_ var type)
       (make-bytevector-mapping var (c-sizeof type)))))

  (define-syntax define-c-enum
    (lambda (x)
      (syntax-case x ()
        ((stx . elts)
         (let ((exact-integer? (lambda (x) (and (integer? x) (exact? x)))))
           (let loop ((n 0) (defs '()) (elts (syntax->datum #'elts)))
             (destructuring-match elts
               (()
                (datum->syntax #'stx (cons 'begin (reverse defs))))
               ((([? symbol? name] . [? exact-integer? n]) . more)
                (loop (+ n 1) (cons `(define ,name ,n) defs) more))
               (([? symbol? name] . more)
                (loop (+ n 1) (cons `(define ,name ,n) defs) more))
               (_
                (syntax-violation 'define-c-enum "invalid syntax" x (car elts))))))))))

  (let-syntax
    ((define-methods
      (lambda (x)
        (syntax-case x ()
          ((stx base size)
           (let ((base (syntax->datum #'base)))
             (define make-id (lambda (s) (datum->syntax #'stx (string->symbol (format s base)))))
             (with-syntax
                 ((constructor (make-id "make-c-~a"))
                  (accessor (make-id "c-~a-ref"))
                  (mutator (make-id "c-~a-set!"))
                  (bv-accessor (make-id "bytevector-c-~a-ref"))
                  (bv-mutator (make-id "bytevector-c-~a-set!")))
               #'(begin
                   (define constructor
                     (lambda (val)
                       (let ((obj (make-bytevector size)))
                         (bv-mutator obj 0 val) obj)))
                   (define accessor
                     (lambda (obj)
                       (bv-accessor
                        (if (bytevector? obj) obj (make-bytevector-mapping obj size))
                        0)))
                   (define mutator
                     (lambda (obj val)
                       (bv-mutator
                        (if (bytevector? obj) obj (make-bytevector-mapping obj size))
                        val 0))))))))))
     (define-accessor
      (lambda (x)
        (syntax-case x ()
          ((stx base size)
           (let ((base (syntax->datum #'base)))
             (define make-id (lambda (s) (datum->syntax #'stx (string->symbol (format s base)))))
             (with-syntax ((accessor (make-id "c-~a-ref")) (bv-accessor (make-id "bytevector-c-~a-ref")))
               #'(define accessor
                   (lambda (obj)
                     (bv-accessor
                      (if (bytevector? obj) obj (make-bytevector-mapping obj size))
                      0))))))))))
    (define-methods bool sizeof:bool)
    (define-methods short sizeof:short)
    (define-methods int sizeof:int)
    (define-methods long sizeof:long)
    (define-methods long-long sizeof:long-long)
    (define-methods void* sizeof:void*)
    (define-methods float 4)
    (define-methods double 8)
    (define-methods int8 1)
    (define-methods int16 2)
    (define-methods int32 3)
    (define-methods int64 4)
    (define-accessor uint8 1)
    (define-accessor uint16 2)
    (define-accessor uint32 3)
    (define-accessor uint64 4)
    (define-accessor unsigned-short sizeof:short)
    (define-accessor unsigned-int sizeof:int)
    (define-accessor unsigned-long sizeof:long)
    (define-accessor unsigned-long-long sizeof:long-long))

  (define make-c-string (lambda (s) (string->utf8/nul s)))

  (define c-string-ref
    (lambda (obj)
      (if (bytevector? obj)
          (let ((n (bytevector-c-strlen obj)))
            (if (= n (bytevector-length obj))
                (utf8->string obj)
                (let ((bv (make-bytevector n)))
                  (bytevector-copy! obj 0 bv 0 n)
                  (utf8->string bv))))
          (let loop ((m 1024))
            (let ((bv (make-bytevector-mapping obj m)))
              (let ((n (bytevector-c-strlen bv)))
                (if (= n (bytevector-length bv))
                    (loop (+ m m))
                    (utf8->string (make-bytevector-mapping obj n)))))))))

  (define c-string-set!
    (lambda (obj s)
      (let ((src (string->utf8/nul s)))
        (let ((dst (if (bytevector? obj) obj (make-bytevector-mapping obj (bytevector-length src)))))
          (bytevector-copy! src 0 dst 0 (bytevector-length src))))))

  ) ;[end]
