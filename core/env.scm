(define *current-macro-environment* (make-eq-hashtable))
(define *current-variable-environment* (make-eq-hashtable))

(define (environment-macro-set! name transformer)
  (hashtable-delete! *current-macro-environment* name)
  (hashtable-set! *current-macro-environment* name transformer))

(define (environment-macro-ref name)
  (hashtable-ref *current-macro-environment* name #f))

(define (environment-macro-contains? name)
  (hashtable-contains? *current-macro-environment* name))

(define (environment-variable-contains? name)
  (hashtable-contains? *current-variable-environment* name))

(define (environment-variable-set! name transformer)
  (hashtable-delete! *current-variable-environment* name)
  (hashtable-set! *current-variable-environment* name transformer))

(define (environment-variable-ref name)
  (or (environment-variable-contains? name) (error "environment-variable-ref: symbol not found" name))
  (hashtable-ref *current-variable-environment* name #f))
