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
   (define (equal-hash obj) (hash obj)))
  (ypsilon
   (define (uuid) (make-uuid))
   (define (make-equal-hashtable) (make-hashtable equal-hash equal?))
   (define (hashtable->alist ht)
     (let-values (((keys vals) (hashtable-entries ht)))
       (map cons (vector->list keys) (vector->list vals)))))
  (else))
