;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define extend-env
  (lambda (bindings env)
    (if (null? bindings)
        env
        (append bindings env))))

(define env-lookup
  (lambda (env id)
    (or (symbol? id) (scheme-error "internal error: env-lookup: expect symbol but got ~s" id))
    (cond ((assq id env)
           =>
           (lambda (binding)
             (cond ((import? (cdr binding))
                    (let ((extern (cddr binding))) (core-hashtable-ref (current-macro-environment) extern extern)))
                   (else (cdr binding)))))
          (else (core-hashtable-ref (current-macro-environment) id id)))))

(define env-delete!
  (lambda (env id)
    (cond ((assq id env)
           => (lambda (binding)
                (set-car! binding #f)
                (set-cdr! binding 'no-use))))))

(define unrename-syntax
  (lambda (form env)
    (let loop ((lst form))
      (cond ((pair? lst)
             (let ((na (loop (car lst))) (nd (loop (cdr lst))))
               (cond ((and (eq? na (car lst)) (eq? nd (cdr lst))) lst)
                     ((annotated? lst) (annotate (cons na nd) lst))
                     (else (cons na nd)))))
            ((renamed-id? lst)
             (let ((deno (env-lookup env lst)))
               (cond ((special? deno) (cond ((eq? deno denote-_) '_) (else (core-primitive-name (original-id lst)))))
                     ((macro? deno)
                      (let ((id (original-id lst)))
                        (cond ((eq? deno (env-lookup env id)) id)
                              ((eq? deno (core-hashtable-ref (current-macro-environment) id #f)) id)
                              (else lst))))
                     (else lst))))
            ((vector? lst) (list->vector (map loop (vector->list lst))))
            (else lst)))))

(define lookup-lexical-name
  (lambda (id env)
    (let ((deno (env-lookup env id)))
      (cond ((symbol? deno) deno)
            ((and (macro? deno) (assq deno env)) => cdr)
            (else id)))))

(define lookup-topmost-subst
  (lambda (id env)
    (define unrename-primitive-id
      (lambda (id)
        (if (and (eq? (symbol-contains id (current-primitive-prefix)) 0)
                 (core-hashtable-contains? (current-variable-environment) id))
            (let ((name (symbol->string id))) (string->symbol (substring name 1 (string-length name))))
            id)))
    (if (symbol? id)
        (let ((deno (env-lookup env id)))
          (cond ((uninterned-symbol? deno)
                 (cond ((local-macro-symbol? deno) deno)
                       ((renamed-variable-id? deno) deno)
                       ((eq? id deno) (unrename-primitive-id (original-id id)))
                       (else (lookup-topmost-subst deno env))))
                ((symbol? deno) (unrename-primitive-id (core-hashtable-ref (current-top-level-renames) deno deno)))
                ((unbound? deno) (unrename-primitive-id (original-id id)))
                ((and (macro? deno) (assq deno env)) => cdr)
                (else deno)))
        (let ((ren (syntax-object-renames id)))
          (if (pair? ren)
              (if (symbol? (cdr ren)) (lookup-topmost-subst (cdr ren) env) (cdr ren))
              (lookup-topmost-subst (syntax-object-expr id) env))))))

(define free-id=?
  (lambda (id1 id2)
    (eq? (lookup-topmost-subst id1 (current-transformer-environment))
         (lookup-topmost-subst id2 (current-expansion-environment)))))

(define make-import
  (lambda (id)
    (cons 'import id)))

(define make-unbound
  (lambda ()
    '(unbound)))

(define make-out-of-context
  (lambda (template)
    (if template
        (cons 'out-of-context template)
        '(out-of-context . #f))))

(define make-pattern-variable
  (lambda (rank)
    (cons 'pattern-variable rank)))

(define make-macro
  (lambda (spec env)
    (cons* 'macro spec env)))

(define make-macro-variable
  (lambda (spec env)
    (cons* 'macro-variable spec env)))

(define make-special
  (lambda (proc)
    (cons 'special proc)))

(define import?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'import))))

(define unbound?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'unbound))))

(define out-of-context?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'out-of-context))))

(define macro?
  (lambda (den)
    (and (pair? den)
         (or (eq? (car den) 'macro)
             (eq? (car den) 'macro-variable)))))

(define macro-variable?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'macro-variable))))

(define pattern-variable?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'pattern-variable))))

(define special?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'special))))

(define unexpected-unquote
  (lambda (expr env)
    (syntax-violation (car expr) "unquote appear outside of quasiquote" expr)))

(define unexpected-unquote-splicing
  (lambda (expr env)
    (syntax-violation (car expr) "unquote-splicing appear outside of quasiquote" expr)))

(define unexpected-auxiliary-syntax
  (lambda (expr env)
    (syntax-violation (car expr) "misplaced auxiliary syntactic keyword" expr)))

(define unexpected-syntax
  (lambda (expr env)
    (syntax-violation (car expr) "misplaced syntactic keyword" expr)))

(define core-env (make-core-hashtable))

(let ()
  (define init-core-macro
    (lambda (id deno)
      (core-hashtable-set! core-env id deno)
      (core-hashtable-set! core-env (core-primitive-name id) deno)))
  (init-core-macro 'lambda            (make-special expand-lambda))
  (init-core-macro 'quote             (make-special expand-quote))
  (init-core-macro 'if                (make-special expand-if))
  (init-core-macro 'set!              (make-special expand-set!))
  (init-core-macro 'define-syntax     (make-special expand-define-syntax))
  (init-core-macro 'let-syntax        (make-special expand-let-syntax))
  (init-core-macro 'letrec-syntax     (make-special expand-letrec-syntax))
  (init-core-macro 'begin             (make-special expand-begin))
  (init-core-macro 'define            (make-special expand-define))
  (init-core-macro 'quasiquote        (make-special expand-quasiquote))
  (init-core-macro 'let               (make-special expand-let))
  (init-core-macro 'letrec            (make-special expand-letrec))
  (init-core-macro 'let*              (make-special expand-let*))
  (init-core-macro 'cond              (make-special expand-cond))
  (init-core-macro 'case              (make-special expand-case))
  (init-core-macro 'do                (make-special expand-do))
  (init-core-macro 'and               (make-special expand-and))
  (init-core-macro 'or                (make-special expand-or))
  (init-core-macro 'letrec*           (make-special expand-letrec*))
  (init-core-macro 'library           (make-special expand-library))
  (init-core-macro 'define-library    (make-special expand-define-library))
  (init-core-macro 'let*-values       (make-special expand-let*-values))
  (init-core-macro 'let-values        (make-special expand-let-values))
  (init-core-macro 'syntax            (make-special expand-syntax))
  (init-core-macro 'syntax-case       (make-special expand-syntax-case))
  (init-core-macro 'identifier-syntax (make-special expand-identifier-syntax))
  (init-core-macro 'assert            (make-special expand-assert))
  (init-core-macro 'unquote           (make-special unexpected-unquote))
  (init-core-macro 'unquote-splicing  (make-special unexpected-unquote-splicing))
  (init-core-macro 'syntax-rules      (make-special unexpected-syntax))
  (init-core-macro 'else              (make-special unexpected-auxiliary-syntax))
  (init-core-macro '=>                (make-special unexpected-auxiliary-syntax))
  (init-core-macro '...               (make-special unexpected-auxiliary-syntax))
  (init-core-macro '_                 (make-special unexpected-auxiliary-syntax))
  (init-core-macro 'import            (make-special expand-import))
  (init-core-macro 'include           (make-special expand-include))
  (init-core-macro 'include-ci        (make-special expand-include-ci)))

(define denote-lambda           (core-hashtable-ref core-env 'lambda #f))
(define denote-begin            (core-hashtable-ref core-env 'begin #f))
(define denote-define           (core-hashtable-ref core-env 'define #f))
(define denote-define-syntax    (core-hashtable-ref core-env 'define-syntax #f))
(define denote-let-syntax       (core-hashtable-ref core-env 'let-syntax #f))
(define denote-letrec-syntax    (core-hashtable-ref core-env 'letrec-syntax #f))
(define denote-quasiquote       (core-hashtable-ref core-env 'quasiquote #f))
(define denote-quote            (core-hashtable-ref core-env 'quote #f))
(define denote-if               (core-hashtable-ref core-env 'if #f))
(define denote-set!             (core-hashtable-ref core-env 'set! #f))
(define denote-unquote          (core-hashtable-ref core-env 'unquote #f))
(define denote-unquote-splicing (core-hashtable-ref core-env 'unquote-splicing #f))
(define denote-let              (core-hashtable-ref core-env 'let #f))
(define denote-let*             (core-hashtable-ref core-env 'let* #f))
(define denote-cond             (core-hashtable-ref core-env 'cond #f))
(define denote-or               (core-hashtable-ref core-env 'or #f))
(define denote-letrec*          (core-hashtable-ref core-env 'letrec* #f))
(define denote-syntax-quote     (core-hashtable-ref core-env 'syntax-quote #f))
(define denote-syntax           (core-hashtable-ref core-env 'syntax #f))
(define denote-syntax-case      (core-hashtable-ref core-env 'syntax-case #f))
(define denote-syntax-rules     (core-hashtable-ref core-env 'syntax-rules #f))
(define denote-else             (core-hashtable-ref core-env 'else #f))
(define denote-=>               (core-hashtable-ref core-env '=> #f))
(define denote-_                (core-hashtable-ref core-env '_ #f))
(define denote-import           (core-hashtable-ref core-env 'import #f))

(define denote-lambda?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-lambda))))

(define denote-begin?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-begin))))

(define denote-let?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-let))))

(define denote-quote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-quote))))

(define denote-quasiquote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-quasiquote))))

(define denote-unquote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-unquote))))

(define denote-unquote-splicing?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-unquote-splicing))))

(define denote-syntax-rules?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-syntax-rules))))

(define denote-else?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-else))))

(define denote-=>?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-=>))))

(define denote-set!?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-set!))))

(define private-primitives-environment
  (list
    (cons '|.list| '|.list|)
    (cons '|.cons| '|.cons|)
    (cons '|.cons*| '|.cons*|)
    (cons '|.append| '|.append|)
    (cons '|.vector| '|.vector|)
    (cons '|.list->vector| '|.list->vector|)
    (cons '|.eq?| '|.eq?|)
    (cons '|.eqv?| '|.eqv?|)
    (cons '|.memq| '|.memq|)
    (cons '|.memv| '|.memv|)
    (cons '|.call-with-values| '|.call-with-values|)
    (cons '|.identifier?| '|.identifier?|)
    (cons '|.make-variable-transformer| '|.make-variable-transformer|)
    (cons '|.unspecified| '|.unspecified|)))
(current-macro-environment core-env)
