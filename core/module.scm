(define-record-type <module>
  (make-module macro variable)
  module?
  (macro module-macro module-macro-set!)
  (variable module-variable module-variable-set!))

(define *current-module* (make-module (make-eq-hashtable) (make-eq-hashtable)))

(define (global-macro-set! name transformer)
  (hashtable-delete! (module-variable *current-module*) name)
  (hashtable-set! (module-macro *current-module*) name transformer))

(define (global-macro-ref name)
  (hashtable-ref (module-macro *current-module*) name #f))

(define (global-variable-set! name transformer)
  (hashtable-delete! (module-macro *current-module*) name)
  (hashtable-set! (module-variable *current-module*) name transformer))

(define (global-variable-ref name)
  (hashtable-ref (module-variable *current-module*) name #f))
