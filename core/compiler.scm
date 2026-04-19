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
  (let ((ht (make-eq-hashtable)))
    (let walk ((e expr) (s bound-vars))
      (cond ((symbol? e) (unless (memq e s) (hashtable-set! ht e #t)))
            ((not (pair? e)) #f)
            (else
             (if (and (symbol? (car e)) (eq? (environment-macro-ref (car e)) 'builtin))
                 (cond ((eq? (car e) 'quote) #f)
                       ((eq? (car e) 'if)
                        (walk (cadr e) s) (walk (caddr e) s) (unless (null? (cdddr e)) (walk (cadddr e) s)))
                       ((eq? (car e) 'begin) (for-each (lambda (x) (walk x s)) (cdr e)))
                       ((eq? (car e) 'set!) (unless (memq (cadr e) s) (hashtable-set! ht (cadr e) #t)) (walk (caddr e) s))
                       ((eq? (car e) 'define)
                        (let ((head (cadr e)))
                          (if (pair? head)
                              (walk `(lambda ,(cdr head) ,@(cddr e)) (cons (car head) s))
                              (walk (caddr e) (cons head s)))))
                       ((eq? (car e) 'lambda)
                        (let* ((params (cadr e))
                               (new-s (append (flatten-params params) s)))
                          (for-each (lambda (x) (walk x new-s)) (cddr e))))
                       ((eq? (car e) 'let)
                        (let* ((bindings (cadr e))
                               (vars (map car bindings))
                               (vals (map cadr bindings)))
                          (for-each (lambda (v) (walk v s)) vals)
                          (for-each (lambda (x) (walk x (append vars s))) (cddr e))))
                       ((eq? (car e) 'letrec*)
                        (let* ((bindings (cadr e))
                               (vars (map car bindings))
                               (new-s (append vars s)))
                          (for-each (lambda (b) (walk (cadr b) new-s)) bindings)
                          (for-each (lambda (x) (walk x new-s)) (cddr e))))
                       (else (for-each (lambda (x) (walk x s)) e)))
                 (for-each (lambda (x) (walk x s)) e)))))
    (map car (hashtable->alist ht))))

;; Analysis 2: Mutated Variables
(define (analyze-mutated-vars-compiler expr)
  (let ((ht (make-eq-hashtable)))
    (let walk ((e expr))
      (cond ((pair? e)
             (case (car e)
               ((quote) #f)
               ((set!) (hashtable-set! ht (cadr e) #t) (walk (caddr e)))
               ((lambda) (for-each walk (cddr e)))
               ((letrec*)
                (for-each (lambda (b) (walk (cadr b))) (cadr e))
                (for-each walk (cddr e)))
               (else (for-each walk e))))
            (else #f)))
    (map car (hashtable->alist ht))))

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
            (fold (lambda (e acc) (max acc (analyze-max-outgoing-args e))) 0 (cdr expr)))
           ((set!)
            (analyze-max-outgoing-args (caddr expr)))
           ((lambda) 0)
           ((let)
            (let ((bindings (cadr expr))
                  (body (cddr expr)))
              (max (fold (lambda (b acc) (max acc (analyze-max-outgoing-args (cadr b)))) 0 bindings)
                   (fold (lambda (e acc) (max acc (analyze-max-outgoing-args e))) 0 body))))
           ((letrec*)
            (let ((bindings (cadr expr))
                  (body (cddr expr)))
              (max (fold (lambda (b acc) (max acc (analyze-max-outgoing-args (cadr b)))) 0 bindings)
                   (fold (lambda (e acc) (max acc (analyze-max-outgoing-args e))) 0 body))))
           (else
            (let ((args (cdr expr))
                  (proc (car expr)))
              (max (length args)
                   (analyze-max-outgoing-args proc)
                   (fold (lambda (e acc) (max acc (analyze-max-outgoing-args e))) 0 args))))))
        (else
         (let ((args (cdr expr))
               (proc (car expr)))
           (max (length args)
                (analyze-max-outgoing-args proc)
                (fold (lambda (e acc) (max acc (analyze-max-outgoing-args e))) 0 args))))))

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

;; Pre-intern register symbols r0..r127 at top level so they are permanently
;; rooted in the global environment and cannot be collected by the concurrent GC
;; even when a globals-only root snapshot is taken during JIT-compiled compilation.
;; This eliminates the race where make-reg allocates a symbol that only lives on
;; the JIT call stack until it is stored into the compiler context.
(define *pre-interned-regs*
  (let loop ((i 0) (acc '()))
    (if (> i 127)
        (list->vector (reverse acc))
        (loop (+ i 1)
              (cons (string->symbol (string-append "r" (number->string i))) acc)))))

(define (make-reg i)
  (if (and (>= i 0) (< i (vector-length *pre-interned-regs*)))
      (vector-ref *pre-interned-regs* i)
      (string->symbol (string-append "r" (number->string i)))))

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
                       (compiler-ctx-emit! ctx '(unspecified r0))
                       (if tail? (compiler-ctx-emit! ctx `(ret)))))
                    ((eq? (car expr) 'if) (codegen-if expr env next-reg tail? ctx))
                    ((eq? (car expr) 'set!) (codegen-set! expr env next-reg tail? ctx))
                    ((eq? (car expr) 'lambda) (codegen-lambda expr env next-reg tail? ctx))
                    ((eq? (car expr) 'let) (codegen-let expr env next-reg tail? ctx))
                    ((eq? (car expr) 'letrec*) (codegen-letrec* expr env next-reg tail? ctx))
                    (else (codegen-application expr env next-reg tail? ctx)))
                (codegen-application expr env next-reg tail? ctx)))))

(define (codegen-args args env arg-base ctx)
  ;; Compile each argument first (may trigger GC), THEN allocate temp-reg.
  ;; A symbol allocated before codegen would only be reachable from the JIT
  ;; call stack during sub-compilation — invisible to a globals-only GC snapshot.
  (let loop ((as args) (i 0))
    (if (not (null? as))
        (begin
          (codegen (car as) env (+ arg-base i 1) #f ctx)
          (let ((temp-reg (make-reg (+ arg-base i))))
            (compiler-ctx-emit! ctx `(mov ,temp-reg r0))
            (loop (cdr as) (+ i 1)))))))

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
  ;; Compile the test first (may trigger GC), then allocate labels immediately
  ;; before they are stored into ctx.  This follows the same principle as the
  ;; codegen-application fix: a freshly-allocated symbol that only lives on the
  ;; JIT call stack can be swept when the concurrent GC takes a globals-only
  ;; root snapshot during one of the recursive codegen calls.
  (codegen (cadr expr) env next-reg #f ctx)
  ;; Allocate t-label and f-label after test compilation, store them at once.
  (let ((t-label (gen-label ctx))
        (f-label (gen-label ctx)))
    (compiler-ctx-emit! ctx `(if ,t-label ,f-label))
    (compiler-ctx-emit! ctx `(label ,t-label))
    ;; Compile the then-branch.  end-label is allocated afterwards to avoid a
    ;; GC window where it would only be reachable from the JIT stack.
    (codegen (caddr expr) env next-reg tail? ctx)
    (if (not tail?)
        ;; Non-tail: allocate end-label now that then-branch compilation is done.
        (let ((end-label (gen-label ctx)))
          (compiler-ctx-emit! ctx `(jump ,end-label))
          (compiler-ctx-emit! ctx `(label ,f-label))
          (if (null? (cdddr expr))
              (compiler-ctx-emit! ctx `(const r0 #f))
              (codegen (cadddr expr) env next-reg #f ctx))
          (compiler-ctx-emit! ctx `(label ,end-label)))
        ;; Tail: no end-label needed.
        (begin
          (compiler-ctx-emit! ctx `(label ,f-label))
          (if (null? (cdddr expr))
              (begin (compiler-ctx-emit! ctx `(const r0 #f))
                     (compiler-ctx-emit! ctx `(ret)))
              (codegen (cadddr expr) env next-reg tail? ctx))))))

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
             (compiler-ctx-emit! ctx `(global-set! ,var r0))
             (compiler-ctx-emit! ctx '(unspecified r0))))
      (if tail? (compiler-ctx-emit! ctx `(ret))))))

(define (codegen-lambda expr env next-reg tail? ctx)
  (let* ((params (cadr expr))
         (body (cddr expr))
         (all-free (analyze-free-vars-compiler expr '()))
         (params-list (flatten-params params))
         (potential-free (remove-from-list all-free params-list))
         (free (filter (lambda (f)
                         (let ((b (lookup f env)))
                           (and (not (eq? (car b) 'global))
                                (not (eq? (car b) 'self)))))
                       potential-free))
         (prev-code (compiler-ctx-code ctx))
         (max-outgoing (analyze-max-outgoing-args `(begin ,@body))))
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
         (let ((entry-label (gen-label ctx 'closure)))
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
                 (begin (compiler-ctx-emit! ctx `(make-closure r0 ,entry-label ,(reverse regs) ,n-fixed ,has-rest?))
                        (if tail? (compiler-ctx-emit! ctx `(ret))))
                 (let ((b (lookup (car fs) env)))
                   (if (eq? (car b) 'reg)
                       (loop (cdr fs) (cons (cdr b) regs) r)
                       (let ((tmp (make-reg r)))
                         (if (eq? (car b) 'self)
                             (compiler-ctx-emit! ctx `(closure-self ,tmp))
                             (compiler-ctx-emit! ctx `(closure-ref ,tmp ,(cdr b))))
                         (loop (cdr fs) (cons tmp regs) (+ r 1))))))))))))

(define (codegen-application expr env next-reg tail? ctx)
  (let* ((proc (car expr)) (args (cdr expr)) (num-args (length args)) (base-reg (max 1 next-reg)))
    (codegen-args args env base-reg ctx)
    ;; Compile proc before allocating call-reg: codegen may trigger GC, and a
    ;; freshly-allocated symbol held only on the JIT call stack can be swept if
    ;; the concurrent GC takes a globals-only root snapshot before the symbol is
    ;; stored into the compiler context.  By allocating call-reg after all
    ;; sub-compilation is done we minimise this window; the *pre-interned-regs*
    ;; vector above eliminates it entirely for r0..r127.
    (if (and (pair? proc) (eq? (car proc) 'lambda))
        (codegen-lambda proc env (+ base-reg num-args) #f ctx)
        (codegen proc env (+ base-reg num-args) #f ctx))
    (let ((call-reg (make-reg (+ base-reg num-args))))
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
          ;; Compile binding value first (may trigger GC), then allocate reg.
          ;; Allocating reg before codegen leaves it only on the JIT stack
          ;; during sub-compilation — vulnerable to globals-only GC snapshots.
          (begin
            (if (and (pair? (car vs)) (eq? (car (car vs)) 'lambda))
                (codegen-lambda (car vs) env (+ r 1) #f ctx)
                (codegen (car vs) env (+ r 1) #f ctx))
            (let ((reg (make-reg r)))
              (compiler-ctx-emit! ctx `(mov ,reg r0))
              (if (memq (car vars) (compiler-ctx-mutated ctx))
                  (compiler-ctx-emit! ctx `(make-cell ,reg)))
              (loop (cdr vs) (cdr vars) (+ r 1) (cons (cons (car vars) reg) new-scope))))))))

;; Helper: find position of item in list, or #f
(define (list-position item lst)
  (let loop ((l lst) (i 0))
    (cond ((null? l) #f)
          ((eq? (car l) item) i)
          (else (loop (cdr l) (+ i 1))))))

;; Analyze which letrec* vars are forward-referenced by lambda init expressions.
;; A var at position j is forward-referenced if a lambda at position i < j
;; captures var j as a free variable.
(define (analyze-forward-refs vars vals)
  (let ((ht (make-eq-hashtable)))
    (let loop ((i 0) (vs vals))
      (if (null? vs)
          (map car (hashtable->alist ht))
          (begin
            (if (and (pair? (car vs)) (eq? (car (car vs)) 'lambda))
                (let ((fvs (analyze-free-vars-compiler (car vs) '())))
                  (for-each (lambda (fv)
                              (let ((pos (list-position fv vars)))
                                (if (and pos (>= pos i))
                                    (hashtable-set! ht fv #t))))
                            fvs)))
            (loop (+ i 1) (cdr vs)))))))

(define (codegen-letrec* expr env next-reg tail? ctx)
  (let* ((bindings (cadr expr))
         (body (cddr expr))
         (vars (map car bindings))
         (vals (map cadr bindings))
         (n (length vars))
         (mutated (compiler-ctx-mutated ctx))
         (forward-refs (analyze-forward-refs vars vals))
         ;; Vars that need cells: mutated by set! in body OR forward-referenced
         (needs-cell? (lambda (v) (or (memq v mutated) (memq v forward-refs))))
         ;; Assign a register for each binding variable
         (regs (map (lambda (i) (make-reg (+ next-reg i))) (iota n)))
         ;; Create full scope with all vars
         (full-scope (map cons vars regs))
         (full-env (cons full-scope env))
         ;; Register count beyond the letrec* bindings
         (inner-next-reg (+ next-reg n)))
    ;; Add forward-referenced vars to the mutated set so codegen-symbol/codegen-lambda
    ;; uses cell-ref/cell-set for these variables throughout the compiled code
    (vector-set! ctx 4 (append forward-refs mutated))
    ;; For vars needing cells: initialize register to #f and create cell
    ;; For other vars: just initialize register to #f
    (for-each (lambda (var reg)
                (compiler-ctx-emit! ctx `(const r0 #f))
                (compiler-ctx-emit! ctx `(mov ,reg r0))
                (if (needs-cell? var)
                    (compiler-ctx-emit! ctx `(make-cell ,reg))))
              vars regs)
    ;; Process each binding sequentially
    (let loop ((i 0))
      (if (= i n)
          ;; All bindings processed, compile body
          (codegen `(begin ,@body) full-env inner-next-reg tail? ctx)
          (let ((var (list-ref vars i))
                (val (list-ref vals i))
                (reg (list-ref regs i)))
            ;; Compile init expression in full env
            (if (and (pair? val) (eq? (car val) 'lambda))
                (codegen-lambda val full-env inner-next-reg #f ctx)
                (codegen val full-env inner-next-reg #f ctx))
            ;; Store result: for cell vars, set the cell content; for others, direct register
            (if (needs-cell? var)
                (compiler-ctx-emit! ctx `(reg-cell-set! ,reg r0))
                (compiler-ctx-emit! ctx `(mov ,reg r0)))
            (loop (+ i 1)))))))

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
