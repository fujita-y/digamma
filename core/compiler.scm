;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; Bytecode compiler for Scheme Core Forms.
;; Compiles core forms into a flat vector of bytecode instructions.

;;=============================================================================
;; SECTION 1: Instruction Sets & Accessors
;;=============================================================================

(define ops-loads
  '(const mov global-ref closure-ref closure-cell-ref reg-cell-ref))

(define ops-memory-pairs
  '((reg-cell-set! . reg-cell-ref)
    (closure-cell-set! . closure-cell-ref)
    (closure-set! . closure-ref)))

(define (inst-op inst) (car inst))
(define (inst-arg1 inst) (cadr inst))
(define (inst-arg2 inst) (caddr inst))

;;=============================================================================
;; SECTION 2: Parameter Analysis
;;=============================================================================

(define (flatten-params params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (flatten-params (cdr params))))
        (else '())))

(define (analyze-params params)
  (let loop ((p params) (fixed '()))
    (cond ((null? p)
           (list (reverse fixed) #f #f))
          ((symbol? p)
           (list (reverse fixed) p #t))
          ((pair? p)
           (loop (cdr p) (cons (car p) fixed)))
          (else
           (error "Invalid parameter list" params)))))

;;=============================================================================
;; SECTION 3: Variable Analysis
;;=============================================================================

;; Analysis 1: Free Variables
(define (analyze-free-vars-compiler expr bound-vars)
  (cond ((symbol? expr)
         (if (memq expr bound-vars) '() (list expr)))
        ((not (pair? expr)) '())
        (else
         (if (and (symbol? (car expr)) (eq? (environment-macro-ref (car expr)) 'builtin))
             (cond ((eq? (car expr) 'quote) '())
                   ((eq? (car expr) 'if)
                    (set-union (analyze-free-vars-compiler (cadr expr) bound-vars)
                               (analyze-free-vars-compiler (caddr expr) bound-vars)
                               (if (null? (cdddr expr))
                                   '()
                                   (analyze-free-vars-compiler (cadddr expr) bound-vars))))
                   ((eq? (car expr) 'begin)
                    (apply set-union (map (lambda (e) (analyze-free-vars-compiler e bound-vars)) (cdr expr))))
                   ((eq? (car expr) 'set!)
                    (set-union (if (memq (cadr expr) bound-vars) '() (list (cadr expr)))
                               (analyze-free-vars-compiler (caddr expr) bound-vars)))
                   ((eq? (car expr) 'define)
                    (let ((head (cadr expr)))
                      (if (pair? head)
                          (analyze-free-vars-compiler `(lambda ,(cdr head) ,@(cddr expr))
                                                      (cons (car head) bound-vars))
                          (analyze-free-vars-compiler (caddr expr)
                                                      (cons head bound-vars)))))
                   ((eq? (car expr) 'lambda)
                    (let* ((params (cadr expr))
                           (new-bound (flatten-params params)))
                      (set-minus (apply set-union
                                    (map (lambda (e)
                                           (analyze-free-vars-compiler e (append new-bound bound-vars)))
                                    (cddr expr)))
                        (append new-bound bound-vars))))
                   ((eq? (car expr) 'let)
                    (let* ((bindings (cadr expr))
                           (vars (map car bindings))
                           (vals (map cadr bindings)))
                      (set-union (apply set-union 
                                   (map (lambda (v) (analyze-free-vars-compiler v bound-vars)) vals))
                                 (set-minus 
                                   (apply set-union
                                     (map (lambda (e)
                                            (analyze-free-vars-compiler e (append vars bound-vars)))
                                          (cddr expr)))
                                  (append vars bound-vars)))))
                   (else
                    (apply set-union (map (lambda (e) (analyze-free-vars-compiler e bound-vars)) expr))))
             (apply set-union (map (lambda (e) (analyze-free-vars-compiler e bound-vars)) expr))))))

;; Analysis 2: Mutated Variables
(define (analyze-mutated-vars-compiler expr)
  (cond
    ((pair? expr)
     (case (car expr)
       ((quote) '())
       ((set!)
        (cons (cadr expr) (analyze-mutated-vars-compiler (caddr expr))))
       ((lambda)
        (apply set-union (map (lambda (e) (analyze-mutated-vars-compiler e)) (cddr expr))))
       (else
        (apply set-union (map (lambda (e) (analyze-mutated-vars-compiler e)) expr)))))
    (else '())))

;;=============================================================================
;; SECTION 4: Max Outgoing Args Analysis
;;=============================================================================

(define (analyze-max-outgoing-args expr)
  (cond ((not (pair? expr)) 0)
        ((symbol? (car expr))
         (case (car expr)
           ((quote) 0)
           ((if)
            (max (analyze-max-outgoing-args (cadr expr))
                 (analyze-max-outgoing-args (caddr expr))
                 (if (null? (cdddr expr)) 0 (analyze-max-outgoing-args (cadddr expr)))))
           ((begin)
            (apply max 0 (map analyze-max-outgoing-args (cdr expr))))
           ((set!)
            (analyze-max-outgoing-args (caddr expr)))
           ((lambda) 0)
           ((let)
            (let ((bindings (cadr expr))
                  (body (cddr expr)))
              (max (apply max 0 (map (lambda (b) (analyze-max-outgoing-args (cadr b))) bindings))
                   (apply max 0 (map analyze-max-outgoing-args body)))))
           (else
            (let ((args (cdr expr))
                  (proc (car expr)))
              (max (length args)
                   (analyze-max-outgoing-args proc)
                   (apply max 0 (map analyze-max-outgoing-args args)))))))
        (else
         (apply max 0 (map analyze-max-outgoing-args expr)))))

;;=============================================================================
;; SECTION 5: Compiler Context & Register Helpers
;;=============================================================================

(define (make-compiler-context mutated-vars) (vector '() 0 0 '() mutated-vars))
(define (compiler-ctx-code ctx) (vector-ref ctx 0))
(define (compiler-ctx-set-code! ctx code) (vector-set! ctx 0 code))
(define (compiler-ctx-emit! ctx inst) (vector-set! ctx 0 (cons inst (vector-ref ctx 0))))
(define (compiler-ctx-inc-labels! ctx) (vector-set! ctx 1 (+ (vector-ref ctx 1) 1)) (vector-ref ctx 1))
(define (compiler-ctx-inc-closure-labels! ctx) (vector-set! ctx 2 (+ (vector-ref ctx 2) 1)) (vector-ref ctx 2))
(define (compiler-ctx-all-closures ctx) (vector-ref ctx 3))
(define (compiler-ctx-add-closure! ctx code) (vector-set! ctx 3 (cons code (vector-ref ctx 3))))
(define (compiler-ctx-mutated ctx) (vector-ref ctx 4))

(define (gen-label ctx . type)
  (if (and (not (null? type)) (eq? (car type) 'closure))
      (string->symbol (string-append "C" (number->string (compiler-ctx-inc-closure-labels! ctx)) "_"(uuid)))
      (string->symbol (string-append "L" (number->string (compiler-ctx-inc-labels! ctx))))))

(define (make-reg i) (string->symbol (string-append "r" (number->string i))))

(define (lookup var env)
  (let loop ((e env))
    (if (null? e)
        (cons 'global var)
        (let ((scope (car e)))
          (if (and (pair? scope) (eq? (car scope) 'cl))
              (let ((res (assoc var (cdr scope))))
                (if res (cons 'cl (cdr res)) (loop (cdr e))))
              (let ((res (assoc var scope)))
                (if res
                    (if (eq? (cdr res) 'self) (cons 'self #f) (cons 'reg (cdr res)))
                    (loop (cdr e)))))))))

;;=============================================================================
;; SECTION 6: Code Generation
;;=============================================================================

(define (codegen expr env next-reg tail? ctx)
  (cond ((symbol? expr) 
         (codegen-symbol expr env tail? ctx))
        ((not (pair? expr))
         (compiler-ctx-emit! ctx `(const r0 ,expr))
         (if tail? (compiler-ctx-emit! ctx `(ret))))
        (else
          (if (and (symbol? (car expr)) (eq? (environment-macro-ref (car expr)) 'builtin))
              (cond ((eq? (car expr) 'quote)
                     (compiler-ctx-emit! ctx `(const r0 ,(cadr expr)))
                     (if tail? (compiler-ctx-emit! ctx `(ret))))
                    ((eq? (car expr) 'begin)
                     (if (null? (cdr expr))
                         (begin (compiler-ctx-emit! ctx `(const r0 #f)) 
                                (if tail? (compiler-ctx-emit! ctx `(ret))))
                         (let loop ((exprs (cdr expr)))
                           (if (null? (cdr exprs))
                               (codegen (car exprs) env next-reg tail? ctx)
                               (begin (codegen (car exprs) env next-reg #f ctx) 
                                      (loop (cdr exprs)))))))
                    ((eq? (car expr) 'define)
                     (let ((var (cadr expr)) (val (caddr expr)))
                       (codegen val env next-reg #f ctx)
                       (compiler-ctx-emit! ctx `(global-set! ,var r0))
                       (compiler-ctx-emit! ctx `(global-ref r0 ,var))
                       (if tail? (compiler-ctx-emit! ctx `(ret)))))
                    ((eq? (car expr) 'if) (codegen-if expr env next-reg tail? ctx))
                    ((eq? (car expr) 'set!) (codegen-set! expr env next-reg tail? ctx))
                    ((eq? (car expr) 'lambda) (codegen-lambda expr env next-reg tail? ctx))
                    ((eq? (car expr) 'let) (codegen-let expr env next-reg tail? ctx))
                    (else (codegen-application expr env next-reg tail? ctx)))
                (codegen-application expr env next-reg tail? ctx)))))

(define (codegen-args args env arg-base ctx)
  (let loop ((as args) (i 0))
    (if (not (null? as))
        (let ((temp-reg (make-reg (+ arg-base i))))
          (codegen (car as) env (+ arg-base i 1) #f ctx)
          (compiler-ctx-emit! ctx `(mov ,temp-reg r0))
          (loop (cdr as) (+ i 1))))))

(define (move-args num-args base ctx)
  (let move-loop ((j 0))
    (if (< j num-args)
        (begin
          (compiler-ctx-emit! ctx `(mov ,(make-reg j) ,(make-reg (+ base j))))
          (move-loop (+ j 1))))))

(define (codegen-symbol expr env tail? ctx)
  (let ((binding (lookup expr env)))
    (cond ((eq? (car binding) 'reg)
           (if (memq expr (compiler-ctx-mutated ctx))
               (compiler-ctx-emit! ctx `(reg-cell-ref r0 ,(cdr binding)))
               (compiler-ctx-emit! ctx `(mov r0 ,(cdr binding)))))
          ((eq? (car binding) 'cl)
           (if (memq expr (compiler-ctx-mutated ctx))
               (compiler-ctx-emit! ctx `(closure-cell-ref r0 ,(cdr binding)))
               (compiler-ctx-emit! ctx `(closure-ref r0 ,(cdr binding)))))
          ((eq? (car binding) 'self)
           (compiler-ctx-emit! ctx `(closure-self r0)))
          (else
           (compiler-ctx-emit! ctx `(global-ref r0 ,expr))))
    (if tail? (compiler-ctx-emit! ctx `(ret)))))

(define (codegen-if expr env next-reg tail? ctx)
  (let ((t-label (gen-label ctx))
        (f-label (gen-label ctx))
        (end-label (gen-label ctx)))
    (codegen (cadr expr) env next-reg #f ctx)
    (compiler-ctx-emit! ctx `(if ,t-label ,f-label))
    (compiler-ctx-emit! ctx `(label ,t-label))
    (codegen (caddr expr) env next-reg tail? ctx)
    (if (not tail?) (compiler-ctx-emit! ctx `(jump ,end-label)))
    (compiler-ctx-emit! ctx `(label ,f-label))
    (if (null? (cdddr expr))
        (begin (compiler-ctx-emit! ctx `(const r0 #f)) (if tail? (compiler-ctx-emit! ctx `(ret))))
        (codegen (cadddr expr) env next-reg tail? ctx))
    (if (not tail?) (compiler-ctx-emit! ctx `(label ,end-label)))))

(define (codegen-set! expr env next-reg tail? ctx)
  (let ((var (cadr expr))
        (val (caddr expr)))
    (codegen val env next-reg #f ctx)
    (let ((binding (lookup var env)))
      (cond ((eq? (car binding) 'reg)
             (if (memq var (compiler-ctx-mutated ctx))
                 (compiler-ctx-emit! ctx `(reg-cell-set! ,(cdr binding) r0))
                 (compiler-ctx-emit! ctx `(mov ,(cdr binding) r0))))
            ((eq? (car binding) 'cl)
             (compiler-ctx-emit! ctx `(closure-cell-set! ,(cdr binding) r0)))
            (else
             (compiler-ctx-emit! ctx `(global-set! ,var r0))))
      (if tail? (compiler-ctx-emit! ctx `(ret))))))

(define (codegen-lambda expr env next-reg tail? ctx)
  (let* ((params (cadr expr))
         (body (cddr expr))
         (all-free (analyze-free-vars-compiler expr '()))
         (params-list (flatten-params params))
         (potential-free (set-minus all-free params-list))
         (free (filter (lambda (f)
                         (let ((b (lookup f env)))
                           (and (not (eq? (car b) 'global))
                                (not (eq? (car b) 'self)))))
                       potential-free))
         (entry-label (gen-label ctx 'closure))
         (prev-code (compiler-ctx-code ctx))
         (max-outgoing (apply max 0 (map analyze-max-outgoing-args body))))
    (let* ((params-info (analyze-params params))
           (fixed-params (car params-info))
           (rest-param (cadr params-info))
           (has-rest? (caddr params-info)))
       (let* ((n-fixed (length fixed-params))
              (new-base (max 1 max-outgoing))
              (all-params (if has-rest? (append fixed-params (list rest-param)) fixed-params))
              (n-total-params (length all-params))
              (new-scope (let loop ((p all-params) (i 0))
                           (if (null? p) '() (cons (cons (car p) (make-reg (+ new-base i))) (loop (cdr p) (+ i 1))))))
              (cl-scope (let loop ((f free) (i 0))
                          (if (null? f) '() (cons (cons (car f) i) (loop (cdr f) (+ i 1)))))))
         (compiler-ctx-set-code! ctx '())
         (compiler-ctx-emit! ctx `(label ,entry-label))
         (let move-params ((i (- n-total-params 1)))
           (if (>= i 0)
               (begin (compiler-ctx-emit! ctx `(mov ,(make-reg (+ new-base i)) ,(make-reg i))) (move-params (- i 1)))))
         (let* ((self-bindings (if (null? env) '()
                                   (let ((scope (car env)))
                                     (if (and (pair? scope) (not (memq (car scope) '(cl num-params))))
                                         (filter (lambda (b) (eq? (cdr b) 'self)) scope)
                                         '())))))
           (for-each (lambda (p) (if (memq (car p) (compiler-ctx-mutated ctx)) (compiler-ctx-emit! ctx `(make-cell ,(cdr p))))) new-scope)
           (codegen `(begin ,@body) (cons (list (cons 'num-params n-total-params)) (cons new-scope (cons self-bindings (list (cons 'cl cl-scope))))) (+ new-base n-total-params) #t ctx))
         (compiler-ctx-add-closure! ctx (reverse (compiler-ctx-code ctx)))
         (compiler-ctx-set-code! ctx prev-code)
         (let loop ((fs free) (regs '()) (r next-reg))
           (if (null? fs)
               (begin (compiler-ctx-emit! ctx `(make-closure r0 ,entry-label ,(reverse regs) ,n-fixed ,has-rest?)) (if tail? (compiler-ctx-emit! ctx `(ret))))
               (let ((b (lookup (car fs) env)))
                 (if (eq? (car b) 'reg)
                     (loop (cdr fs) (cons (cdr b) regs) r)
                     (let ((tmp (make-reg r)))
                       (if (eq? (car b) 'self)
                           (compiler-ctx-emit! ctx `(closure-self ,tmp))
                           (compiler-ctx-emit! ctx `(closure-ref ,tmp ,(cdr b))))
                       (loop (cdr fs) (cons tmp regs) (+ r 1)))))))))))

(define (codegen-application expr env next-reg tail? ctx)
  (let* ((proc (car expr)) (args (cdr expr)) (num-args (length args)) (base-reg (max 1 next-reg)))
    (codegen-args args env base-reg ctx)
    (let ((call-reg (make-reg (+ base-reg num-args))))
      (if (and (pair? proc) (eq? (car proc) 'lambda))
          (codegen-lambda proc env (+ base-reg num-args) #f ctx)
          (codegen proc env (+ base-reg num-args) #f ctx))
      (compiler-ctx-emit! ctx `(mov ,call-reg r0))
      (move-args num-args base-reg ctx)
      (if tail?
          (compiler-ctx-emit! ctx `(tail-call ,call-reg ,num-args))
          (compiler-ctx-emit! ctx `(call ,call-reg ,num-args))))))

(define (codegen-let expr env next-reg tail? ctx)
  (let* ((bindings (cadr expr)) (body (cddr expr)) (vars (map car bindings)) (vals (map cadr bindings)))
    (let loop ((vs vals) (vars vars) (r next-reg) (new-scope '()))
      (if (null? vs)
          (codegen `(begin ,@body) (cons new-scope env) r tail? ctx)
          (let ((reg (make-reg r)))
            (if (and (pair? (car vs)) (eq? (car (car vs)) 'lambda))
                (codegen-lambda (car vs) env (+ r 1) #f ctx)
                (codegen (car vs) env (+ r 1) #f ctx))
            (compiler-ctx-emit! ctx `(mov ,reg r0))
            (if (memq (car vars) (compiler-ctx-mutated ctx))
                (compiler-ctx-emit! ctx `(make-cell ,reg)))
            (loop (cdr vs) (cdr vars) (+ r 1) (cons (cons (car vars) reg) new-scope)))))))

;;=============================================================================
;; SECTION 7: Main Compiler
;;=============================================================================

(define (compile expr)
  (let* ((mutated (analyze-mutated-vars-compiler expr))
         (ctx (make-compiler-context mutated)))
    (codegen expr '() (max 1 (analyze-max-outgoing-args expr)) #f ctx)
    (compiler-ctx-emit! ctx `(ret))
    (let* ((main-code (reverse (compiler-ctx-code ctx)))
           (closure-code (apply append (compiler-ctx-all-closures ctx))))
      (append main-code closure-code))))
