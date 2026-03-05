(define-record-type <core-environment>
  (make-core-environment macro variable)
  core-environment?
  (macro core-environment-macro core-environment-macro-set!)
  (variable core-environment-variable core-environment-variable-set!))

(define *current-core-environment* (make-core-environment (make-eq-hashtable) (make-eq-hashtable)))

(define (environment-macro-set! name transformer)
  (hashtable-delete! (core-environment-variable *current-core-environment*) name)
  (hashtable-set! (core-environment-macro *current-core-environment*) name transformer))

(define (environment-macro-ref name)
  (hashtable-ref (core-environment-macro *current-core-environment*) name #f))

(define (environment-variable-set! name transformer)
  (hashtable-delete! (core-environment-macro *current-core-environment*) name)
  (hashtable-set! (core-environment-variable *current-core-environment*) name transformer))

(define (environment-variable-ref name)
  (hashtable-ref (core-environment-variable *current-core-environment*) name #f))
