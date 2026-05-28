;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-module (core records)

(export make-record-type-descriptor
        record-type-descriptor?
        make-record-constructor-descriptor
        record-constructor
        record-predicate
        record-accessor
        record-mutator
        record?
        record-rtd
        record-type-name
        record-type-parent
        record-type-uid
        record-type-generative?
        record-type-sealed?
        record-type-opaque?
        record-type-field-names
        record-field-mutable?
        record-type-descriptor
        record-constructor-descriptor
        define-record-type
        fields
        mutable
        immutable
        parent
        protocol
        sealed
        opaque
        nongenerative
        parent-rtd)

(import (core let-values)
        (core destructuring))

;;;

(define wrong-type-argument-message
  (lambda (expect got . nth)
    (if (null? nth)
        (format "expected ~a, but got ~a" expect got)
        (format "expected ~a, but got ~a, as argument ~a" expect got (car nth)))))

;;;

(define nongenerative-record-types (make-eq-hashtable))

(define make-rtd
  (lambda (name parent uid sealed? opaque? fields)
    (tuple 'type:record-type-descriptor name parent uid sealed? opaque? fields)))

(define record-type-descriptor?
  (lambda (obj)
    (and (tuple? obj)
         (eq? (tuple-ref obj 0) 'type:record-type-descriptor))))

(define rtd-name        (lambda (rtd) (tuple-ref rtd 1)))
(define rtd-parent      (lambda (rtd) (tuple-ref rtd 2)))
(define rtd-uid         (lambda (rtd) (tuple-ref rtd 3)))
(define rtd-sealed?     (lambda (rtd) (tuple-ref rtd 4)))
(define rtd-opaque?     (lambda (rtd) (tuple-ref rtd 5)))
(define rtd-fields      (lambda (rtd) (tuple-ref rtd 6)))

(define rtd-ancestor?
  (lambda (parent rtd)
    (let loop ((rtd rtd))
      (or (eq? parent rtd)
          (and rtd
               (loop (rtd-parent rtd)))))))

(define rtd-inherited-field-count
  (lambda (rtd)
    (let loop ((rtd (rtd-parent rtd)) (count 0))
      (cond (rtd
             (loop (rtd-parent rtd)
                   (+ count (length (rtd-fields rtd)))))
            (else
             count)))))

(define rtd-total-field-count
  (lambda (rtd)
    (+ (rtd-inherited-field-count rtd) (length (rtd-fields rtd)))))

(define record-type-name
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (rtd-name rtd)))

(define record-type-parent
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (rtd-parent rtd)))

(define record-type-uid
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (rtd-uid rtd)))

(define record-type-generative?
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (not (rtd-uid rtd))))

(define record-type-sealed?
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (rtd-sealed? rtd)))

(define record-type-opaque?
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (rtd-opaque? rtd)))

(define record-type-field-names
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd)))
    (list->vector (map cdr (rtd-fields rtd)))))

(define record-field-mutable?
  (lambda (rtd k)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-type-name (wrong-type-argument-message "record-type-descriptor" rtd 1) (list rtd k)))
    (car (list-ref (rtd-fields rtd) k))))

(define make-record-type-descriptor
  (lambda (name parent uid sealed? opaque? fields)
    (or (symbol? name)
        (assertion-violation
          'make-record-type-descriptor
          (wrong-type-argument-message "symbol" name 1)
          (list name parent uid sealed? opaque? fields)))
    (or (vector? fields)
        (assertion-violation
          'make-record-type-descriptor
          (wrong-type-argument-message "vector" fields 6)
          (list name parent uid sealed? opaque? fields)))
    (and parent
         (or (record-type-descriptor? parent)
             (assertion-violation
               'make-record-type-descriptor
               (wrong-type-argument-message "record-type descriptor or #f" parent 2)
               (list name parent uid sealed? opaque? fields)))
         (and (rtd-sealed? parent)
              (assertion-violation 'make-record-type-descriptor "attempt to extend a sealed record-type" parent)))
    (let ((opaque? (or opaque? (and parent (rtd-opaque? parent))))
          (fields
            (map (lambda (field)
                   (destructuring-match field
                     (('mutable ?name) (cons #t ?name))
                     (('immutable ?name) (cons #f ?name))
                     (_ (assertion-violation 'make-record-type-descriptor "malformed field specifiers" fields))))
                 (vector->list fields))))
      (cond ((not uid) (make-rtd name parent #f sealed? opaque? fields))
            ((hashtable-ref nongenerative-record-types uid #f)
             =>
             (lambda (current)
               (if (and (eqv? uid (rtd-uid current))
                        (eqv? parent (rtd-parent current))
                        (equal? fields (rtd-fields current)))
                   current
                   (assertion-violation
                     'make-record-type-descriptor
                     "mismatched subsequent call for nongenerative record-type"
                     (list name parent uid sealed? opaque? fields)))))
            (else
              (let ((new (make-rtd name parent uid sealed? opaque? fields)))
                (hashtable-set! nongenerative-record-types uid new)
                new))))))

(define make-rcd
  (lambda (rtd protocol custom-protocol? parent)
    (tuple 'type:record-constructor-descriptor rtd protocol custom-protocol? parent)))

(define record-constructor-descriptor?
  (lambda (obj)
    (and (tuple? obj)
         (eq? (tuple-ref obj 0) 'type:record-constructor-descriptor))))

(define rcd-rtd              (lambda (rcd) (tuple-ref rcd 1)))
(define rcd-protocol         (lambda (rcd) (tuple-ref rcd 2)))
(define rcd-custom-protocol? (lambda (rcd) (tuple-ref rcd 3)))
(define rcd-parent           (lambda (rcd) (tuple-ref rcd 4)))

(define default-protocol
  (lambda (rtd)
    (let ((parent (rtd-parent rtd)))
      (if parent
          (let ((parent-field-count (rtd-total-field-count parent)))
            (lambda (p)
              (lambda field-values
                (let-values (((parent-field-values this-field-values) (split-at field-values parent-field-count)))
                  (apply (apply p parent-field-values) this-field-values)))))
          (lambda (p)
            (lambda field-values
              (apply p field-values)))))))

(define make-record-constructor-descriptor
  (lambda (rtd parent protocol)
    (or (record-type-descriptor? rtd)
        (assertion-violation
          'make-record-constructor-descriptor
          (wrong-type-argument-message "record-type-descriptor" rtd 1)
          (list rtd parent protocol)))
    (and parent
         (or (record-constructor-descriptor? parent)
             (assertion-violation
               'make-record-constructor-descriptor
               (wrong-type-argument-message "record-constructor-descriptor or #f" parent 2)
               (list rtd parent protocol))))
    (and protocol
         (or (procedure? protocol)
             (assertion-violation
               'make-record-constructor-descriptor
               (wrong-type-argument-message "procedure or #f" protocol 3)
               (list rtd parent protocol))))
    (and parent
         (or (rtd-parent rtd)
             (assertion-violation
               'make-record-constructor-descriptor
               "mismatch between rtd and parent constructor descriptor"
               rtd
               parent
               protocol)))
    (and parent
         (rtd-parent rtd)
         (or (eq? (rcd-rtd parent) (rtd-parent rtd))
             (assertion-violation
               'make-record-constructor-descriptor
               "mismatch between rtd and parent constructor descriptor"
               rtd
               parent
               protocol)))
    (and protocol
         (rtd-parent rtd)
         (or parent
             (assertion-violation
               'make-record-constructor-descriptor
               "expected #f for protocol since no parent constructor descriptor is provided"
               rtd
               parent
               protocol)))
    (and parent
         (rcd-custom-protocol? parent)
         (or protocol
             (assertion-violation
               'make-record-constructor-descriptor
               "expected procedure for protocol since parent constructor descriptor have custom one"
               rtd
               parent
               protocol)))
    (let ((custom-protocol? (and protocol #t))
          (protocol (or protocol (default-protocol rtd)))
          (parent
            (or parent
                (cond ((rtd-parent rtd) => (lambda (rtd) (make-record-constructor-descriptor rtd #f #f))) (else #f)))))
      (make-rcd rtd protocol custom-protocol? parent))))

(define record?
  (lambda (obj)
    (and (tuple? obj)
         (record-type-descriptor? (tuple-ref obj 0))
         (not (record-type-opaque? (tuple-ref obj 0))))))

(define record-rtd
  (lambda (rec)
    (if (record? rec)
        (tuple-ref rec 0)
        (assertion-violation 'record-rtd (wrong-type-argument-message "non-opaque record" rec)))))

(define make-nested-conser
  (lambda (desc rtd argc)
    ((rcd-protocol desc)
     ((let loop ((desc desc))
        (cond ((rcd-parent desc)
               =>
               (lambda (parent)
                 (lambda extra-field-values
                   (lambda protocol-args
                     (lambda this-field-values
                       (apply
                         ((rcd-protocol parent) (apply (loop parent) (append this-field-values extra-field-values)))
                         protocol-args))))))
              (else
                (lambda extra-field-values
                  (lambda this-field-values
                    (let ((field-values (append this-field-values extra-field-values)))
                      (if (= (length field-values) argc)
                          (apply tuple rtd field-values)
                          (assertion-violation "record constructor" "wrong number of arguments" field-values))))))))))))

(define make-simple-conser
  (lambda (desc rtd argc)
    ((rcd-protocol desc)
     (lambda field-values
       (if (= (length field-values) argc)
           (apply tuple rtd field-values)
           (assertion-violation "record constructor" "wrong number of arguments" field-values))))))

(define flat-field-offset
  (lambda (rtd k)
    (+ (rtd-inherited-field-count rtd) k 1)))

(define make-accessor
  (lambda (rtd k)
    (lambda (obj)
      (cond ((eq? rtd (tuple-ref obj 0)) (tuple-ref obj k))
            ((rtd-ancestor? rtd (tuple-ref obj 0)) (tuple-ref obj k))
            (else
             (assertion-violation "record accessor" (wrong-type-argument-message (format "record of type ~a" (rtd-name rtd)) obj)))))))

(define make-mutator
  (lambda (rtd k)
    (lambda (obj datum)
      (cond ((eq? rtd (tuple-ref obj 0)) (tuple-set! obj k datum))
            ((rtd-ancestor? rtd (tuple-ref obj 0)) (tuple-set! obj k datum))
            (else
             (assertion-violation "record mutator" (wrong-type-argument-message (format "record of type ~a" (rtd-name rtd)) (list obj datum))))))))

(define make-predicate
  (lambda (rtd)
    (lambda (obj)
      (and (tuple? obj)
           (or (eq? rtd (tuple-ref obj 0))
               (rtd-ancestor? rtd (tuple-ref obj 0)))))))

(define record-constructor
  (lambda (desc)
    (or (record-constructor-descriptor? desc)
        (assertion-violation 'record-constructor (wrong-type-argument-message "record-constructor-descriptor" desc)))
    (let ((rtd (rcd-rtd desc)))
      (if (rcd-parent desc)
          (make-nested-conser desc rtd (rtd-total-field-count rtd))
          (make-simple-conser desc rtd (length (rtd-fields rtd)))))))

(define record-predicate
  (lambda (rtd)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-predicate (wrong-type-argument-message "record-type-descriptor" rtd)))
    (make-predicate rtd)))

(define record-accessor
  (lambda (rtd k)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-accssor (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd k)))
    (or (< -1 k (length (rtd-fields rtd)))
        (assertion-violation 'record-accssor "field index out of range"))
    (make-accessor rtd (flat-field-offset rtd k))))

(define record-mutator
  (lambda (rtd k)
    (or (record-type-descriptor? rtd)
        (assertion-violation 'record-mutator (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd k)))
    (or (< -1 k (length (rtd-fields rtd)))
        (assertion-violation 'record-mutator "field index out of range" (list rtd k)))
    (or (record-field-mutable? rtd k)
        (assertion-violation 'record-mutator "specified field is immutable" (list rtd k)))
    (make-mutator rtd (flat-field-offset rtd k))))

(define make-record-type
  (lambda (name rtd rcd)
    (tuple 'type:record-type name rtd rcd)))

(define record-type?
  (lambda (obj)
    (and (tuple? obj)
         (eq? (tuple-ref obj 0) 'type:record-type))))

(define record-type-rtd
  (lambda (obj)
    (or (record-type? obj)
        (assertion-violation 'record-type-rtd (wrong-type-argument-message "record-type" obj)))
    (tuple-ref obj 2)))

(define record-type-rcd
  (lambda (obj)
    (or (record-type? obj)
        (assertion-violation 'record-type-rcd (wrong-type-argument-message "record-type" obj)))
    (tuple-ref obj 3)))

;;;

  (define-syntax define-record-type
    (lambda (x)
      (let ((stash (make-eq-hashtable)))

        (define stash-set!
          (lambda (key value)
            (and (hashtable-ref stash key #f)
                 (syntax-violation 'define-record-type (format "duplicate ~a clause" key) x))
            (hashtable-set! stash key (list value))))

        (define stash-ref
          (lambda (key default)
            (cond ((hashtable-ref stash key #f) => car)
                  (else default))))

        (define parse-record-clauses
          (lambda (first-name record-clauses)

            (define method-name
              (lambda (spec second-name)
                (datum->syntax second-name (string->symbol (format spec first-name (syntax->datum second-name))))))

            (for-each
             (lambda (c)
               (syntax-case c (parent protocol parent-rtd sealed opaque nongenerative fields)
                 ((parent e1)
                  (identifier? #'e1)
                  (stash-set! 'parent #'e1))
                 ((protocol e1)
                  (stash-set! 'protocol #'e1))
                 ((parent-rtd e1 e2)
                  (stash-set! 'parent-rtd (cons #'e1 #'e2)))
                 ((sealed e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'sealed #'e1))
                 ((opaque e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'opaque #'e1))
                 ((nongenerative e1)
                  (identifier? #'e1)
                  (stash-set! 'nongenerative #'e1))
                 ((nongenerative)
                  (stash-set! 'nongenerative (datum->syntax #'k (string->symbol (format "<~a>" (make-uuid))))))
                 ((fields specs ...)
                  (stash-set!
                   'fields
                   (map (lambda (spec)
                          (syntax-case spec (immutable mutable)
                            ((immutable name accessor)
                             (and (identifier? #'name) (identifier? #'accessor))
                             #'((immutable name) accessor #f))
                            ((mutable name accessor mutator)
                             (and (identifier? #'name) (identifier? #'accessor) (identifier? #'mutator))
                             #'((mutable name) accessor mutator))
                            ((immutable name)
                             (identifier? #'name)
                             (with-syntax ((proc (method-name "~a-~a" #'name)))
                               #'((immutable name) proc #f)))
                            ((mutable name)
                             (identifier? #'name)
                             (with-syntax
                                 ((proc1 (method-name "~a-~a" #'name))
                                  (proc2 (method-name "~a-~a-set!" #'name)))
                               #'((mutable name) proc1 proc2)))
                            (name
                             (identifier? #'name)
                             (with-syntax ((proc (method-name "~a-~a" #'name)))
                               #'((immutable name) proc #f)))
                            (_
                             (syntax-violation 'define-record-type "malformed field spec" x spec))))
                        #'(specs ...))))

                 (_ (syntax-violation 'define-record-type "malformed record clauses" x (syntax->datum c)))))
             record-clauses)))

        (syntax-case x ()
          ((_ (record-name constructor-name predicate-name) record-clauses ...)
           (and (identifier? #'record-name) (identifier? #'constructor-name) (identifier? #'predicate-name))
           (begin
             (parse-record-clauses (syntax->datum #'record-name) #'(record-clauses ...))
             (and (stash-ref 'parent-rtd #f)
                  (stash-ref 'parent #f)
                  (syntax-violation 'define-record-type "definition have both parent and parent-rtd clause" x))
             (with-syntax
                 ((record-type
                   (with-syntax ((parent (stash-ref 'parent #f)))
                     (with-syntax ((rtd-parent (cond ((stash-ref 'parent-rtd #f) => car)
                                                     ((stash-ref 'parent #f) #'(record-type-rtd parent))
                                                     (else #f)))
                                   (rcd-parent (cond ((stash-ref 'parent-rtd #f) => cdr)
                                                     ((stash-ref 'parent #f) #'(record-type-rcd parent))
                                                     (else #f)))
                                   (uid (stash-ref 'nongenerative #f))
                                   (sealed (stash-ref 'sealed #f))
                                   (opaque (stash-ref 'opaque #f))
                                   (protocol (stash-ref 'protocol #f))
                                   (((fields _ _) ...) (datum->syntax #'k (stash-ref 'fields '()))))
                       #'(define record-name
                           (let* ((rtd (make-record-type-descriptor 'record-name rtd-parent 'uid sealed opaque '#(fields ...)))
                                  (rcd (make-record-constructor-descriptor rtd rcd-parent protocol)))
                             (make-record-type 'record-name rtd rcd))))))
                  (constructor
                   #'(define constructor-name (record-constructor (record-type-rcd record-name))))
                  (predicate
                   #'(define predicate-name (record-predicate (record-type-rtd record-name))))
                  ((accessors ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
                                    (with-syntax (((_ name _) spec) (n index))
                                      (and (identifier? #'name)
                                           #'(define name (record-accessor (record-type-rtd record-name) n)))))
                                  (stash-ref 'fields '())))))
                  ((mutators ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
                                    (with-syntax (((_ _ name) spec) (n index))
                                      (and (identifier? #'name)
                                           #'(define name (record-mutator (record-type-rtd record-name) n)))))
                                  (stash-ref 'fields '()))))))
               #'(begin record-type constructor predicate accessors ... mutators ...))))
          ((_ record-name record-clauses ...)
           (identifier? #'record-name)
           (let ((base-name (symbol->string (syntax->datum #'record-name))))
             (with-syntax
                 ((constructor-name (datum->syntax #'record-name (string->symbol (string-append "make-" base-name))))
                  (predicate-name (datum->syntax #'record-name (string->symbol (string-append base-name "?")))))
               #'(define-record-type (record-name constructor-name predicate-name) record-clauses ...))))))))

  (define-syntax record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) #'(record-type-rtd name)))))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) #'(record-type-rcd name)))))

  (define-syntax fields
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax mutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax immutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax protocol
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax sealed
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax opaque
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax nongenerative
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent-rtd
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  ) ;[end]