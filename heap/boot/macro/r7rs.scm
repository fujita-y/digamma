;;; Copyright (c) 2004-2019 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define expand-define-library
  (lambda (form env)

    (define permute-env
      (lambda (ht)
        (let loop ((lst (core-hashtable->alist ht)) (bounds '()) (unbounds '()))
          (cond ((null? lst) (append bounds unbounds)) ((unbound? (cdar lst)) (loop (cdr lst) bounds (cons (car lst) unbounds))) (else (loop (cdr lst) (cons (car lst) bounds) unbounds))))))

    (destructuring-match form
      ((_ library-name clauses ...)
       (let ((library-id (library-name->id form library-name)) (library-version (library-name->version form library-name)))
         (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
         (parameterize ((current-include-files (make-core-hashtable)))
             (let ((coreform
                     (let loop ((clauses clauses) (exports '()) (imports '()) (depends '()) (commands '()))
                       (if (null? clauses)
                           (let ((ht-immutables (make-core-hashtable)) (ht-imports (make-core-hashtable)) (ht-publics (make-core-hashtable)))
                             (for-each (lambda (a)
                                         (and (core-hashtable-ref ht-publics (cdr a) #f)
                                              (syntax-violation 'define-library "duplicate export identifiers" (abbreviated-take-form form 4 8) (cdr a)))
                                         (core-hashtable-set! ht-publics (cdr a) #t)
                                         (core-hashtable-set! ht-immutables (car a) #t))
                                       exports)
                             (for-each (lambda (a)
                                         (core-hashtable-set! ht-immutables (car a) #t)
                                         (cond ((core-hashtable-ref ht-imports (car a) #f)
                                                => (lambda (deno)
                                                     (or (eq? deno (cdr a))
                                                         (syntax-violation 'define-library "duplicate import identifiers" (abbreviated-take-form form 4 8) (car a)))))
                                               (else (core-hashtable-set! ht-imports (car a) (cdr a)))))
                                       imports)
                             (let ((ht-env (make-shield-id-table commands)) (ht-libenv (make-core-hashtable)))
                               (for-each (lambda (a)
                                           (core-hashtable-set! ht-env (car a) (cdr a))
                                           (core-hashtable-set! ht-libenv (car a) (cdr a)))
                                         (core-hashtable->alist ht-imports))
                               (parameterize ((current-immutable-identifiers ht-immutables))
                                 (expand-library-body
                                   form
                                   library-id
                                   library-version
                                   commands
                                   exports
                                   imports
                                   depends
                                   (extend-env private-primitives-environment (permute-env ht-env))
                                   (permute-env ht-libenv)))))
                           (destructuring-match clauses
                             ((('export export-spec ...) more ...)
                              (loop more (append exports (parse-exports form export-spec)) imports depends commands))
                             ((('import import-spec ...) more ...)
                              (loop more exports (append imports (parse-imports form import-spec)) (append depends (parse-depends form import-spec)) commands))
                             ((('begin body ...) more ...)
                              (loop more exports imports depends (append commands body)))
                             (_
                              (syntax-violation 'define-library "malformed library declarations" (abbreviated-take-form form 4 8) (car clauses))))))))
               (or (= (core-hashtable-size (current-include-files)) 0)
                   (core-hashtable-set! library-include-dependencies library-id (current-include-files)))
               coreform))))
      (_ (syntax-violation 'define-library "expected library name and declarations" (abbreviated-take-form form 4 8))))))

#|
(define-library
  (foo)
  (export a b c)
  (import (rnrs))
  (begin (define a (lambda (x) (car x))) (define b (lambda (x) (cdr x))))
  (import (rename (core) (current-directory cd)))
  (begin (define c (lambda () (cd)))))
|#