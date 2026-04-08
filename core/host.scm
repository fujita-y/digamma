;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

(cond-expand
  (gauche
   (use rfc.uuid)
   (define (uuid) (uuid->string (uuid4)))
   (define (unspecified) (if #f #f))
   (define (make-eq-hashtable) (make-hash-table 'eq?))
   (define (make-eqv-hashtable) (make-hash-table 'eqv?))
   (define (make-equal-hashtable) (make-hash-table 'equal?))
   (define (make-hashtable hash-fn equiv-fn)
     (cond ((eq? equiv-fn equal?) (make-equal-hashtable))
           ((eq? equiv-fn eqv?) (make-eqv-hashtable))
           ((eq? equiv-fn eq?) (make-eq-hashtable))
           (else (make-hash-table equiv-fn))))
   (define hashtable-clear! hash-table-clear!)
   (define hashtable-contains? hash-table-exists?)
   (define hashtable-delete! hash-table-delete!)
   (define hashtable-ref hash-table-get)
   (define hashtable-set! hash-table-put!)
   (define hashtable->alist hash-table->alist)
   (define (equal-hash obj) (hash obj))
   (define (core-eval expr env) (eval expr env))
   (define (current-environment) (interaction-environment))
   (define (system-environment) (interaction-environment))
   (define (copy-environment-variables! . args) #t))
  (ypsilon
   (import (srfi 1))
   (define (uuid) (make-uuid))
   (define (make-equal-hashtable) (make-hashtable equal-hash equal?))
   (define (hashtable->alist ht)
     (let-values (((keys vals) (hashtable-entries ht)))
       (map cons (vector->list keys) (vector->list vals))))
   (define (core-eval expr env) (eval expr env))
   (define (copy-environment-variables! . args) #t))
   (define fold fold-left)
  (else))

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

(define-syntax with-parameter
  (syntax-rules ()
    ((_ ((param val) ...) body ...)
     (let ((old-vals (list (param) ...))
           (new-vals (list val ...)))
       (for-each (lambda (p v) (p v)) (list param ...) new-vals)
       (let ((results (let () body ...)))
         (for-each (lambda (p v) (p v)) (list param ...) old-vals)
         results)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let-values (((v ...) expr) rest ...) body1 body2 ...)
     (call-with-values
       (lambda () expr)
       (lambda (v ...)
         (let-values (rest ...) body1 body2 ...))))))

