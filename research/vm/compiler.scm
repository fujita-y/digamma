(define-module research.vm.compiler
  (use srfi-1)
  (export compile make-context ctx-extend-n lookup-reg ctx-env)
  )
(select-module research.vm.compiler)

;; --- Utilities ---

(define (set-union s1 s2)
  (if (null? s1) s2
      (let ((rest (set-union (cdr s1) s2)))
        (if (memq (car s1) s2) rest (cons (car s1) rest)))))

(define (set-minus s1 s2)
  (if (null? s1) '()
      (let ((rest (set-minus (cdr s1) s2)))
        (if (memq (car s1) s2) rest (cons (car s1) rest)))))

;; --- Free Variable Analysis ---

(define (analyze-free-vars expr bound-vars)
  (cond
    ((symbol? expr)
     (if (memq expr bound-vars) (list expr) '()))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'if)
     (set-union (analyze-free-vars (cadr expr) bound-vars)
                (set-union (analyze-free-vars (caddr expr) bound-vars)
                           (if (null? (cdddr expr)) '() (analyze-free-vars (cadddr expr) bound-vars)))))
    ((eq? (car expr) 'begin)
     (let loop ((exprs (cdr expr)))
       (if (null? exprs) '()
           (set-union (analyze-free-vars (car exprs) bound-vars)
                      (loop (cdr exprs))))))
    ((eq? (car expr) 'set!)
     (let ((var (cadr expr))
           (val (caddr expr)))
       (set-union (if (memq var bound-vars) (list var) '())
                  (analyze-free-vars val bound-vars))))
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr))
           (body (cddr expr)))
       (let ((new-bound (if (list? params) params
                            (let loop ((p params)) (if (pair? p) (cons (car p) (loop (cdr p))) (list p))))))
         (let ((body-free (let loop ((exprs body))
                            (if (null? exprs) '()
                                (set-union (analyze-free-vars (car exprs) (append new-bound bound-vars))
                                           (loop (cdr exprs)))))))
           (set-minus body-free new-bound)))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (let ((vars (map car bindings))
             (vals (map cadr bindings)))
         (set-union (let loop ((vs vals))
                      (if (null? vs) '()
                          (set-union (analyze-free-vars (car vs) bound-vars)
                                     (loop (cdr vs)))))
                    (let ((body-free (let loop ((exprs body))
                                       (if (null? exprs) '()
                                           (set-union (analyze-free-vars (car exprs) (append vars bound-vars))
                                                      (loop (cdr exprs)))))))
                      (set-minus body-free vars))))))
    (else
     ;; Application
     (let loop ((exprs expr))
       (if (null? exprs) '()
           (set-union (analyze-free-vars (car exprs) bound-vars)
                      (loop (cdr exprs))))))))

;; --- Mutability Analysis & Boxing ---

(define (analyze-mutated-vars expr)
  (cond
    ((pair? expr)
     (cond
       ((eq? (car expr) 'quote) '())
       ((eq? (car expr) 'set!)
        (cons (cadr expr) (analyze-mutated-vars (caddr expr))))
       ((eq? (car expr) 'lambda)
        (let loop ((body (cddr expr)))
          (if (null? body) '()
              (set-union (analyze-mutated-vars (car body)) (loop (cdr body))))))
       ((eq? (car expr) 'let)
         (let ((bindings (cadr expr)) (body (cddr expr)))
           (set-union (let loop ((bs bindings))
                        (if (null? bs) '()
                            (set-union (analyze-mutated-vars (cadr (car bs))) (loop (cdr bs)))))
                      (let loop ((b body))
                        (if (null? b) '()
                            (set-union (analyze-mutated-vars (car b)) (loop (cdr b))))))))
       (else
        (let loop ((exprs expr))
          (if (null? exprs) '()
              (set-union (analyze-mutated-vars (car exprs)) 
                         (loop (cdr exprs))))))))
    (else '())))

(define (box-transform expr mutated-vars bound-vars)
  (cond
    ((symbol? expr)
     (if (and (memq expr bound-vars) (memq expr mutated-vars))
         `(vector-ref ,expr 0)
         expr))
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'if)
     `(if ,(box-transform (cadr expr) mutated-vars bound-vars)
          ,(box-transform (caddr expr) mutated-vars bound-vars)
          ,(if (null? (cdddr expr)) '() (box-transform (cadddr expr) mutated-vars bound-vars))))
    ((eq? (car expr) 'set!)
     (let ((var (cadr expr))
           (val (box-transform (caddr expr) mutated-vars bound-vars)))
       (if (and (memq var bound-vars) (memq var mutated-vars))
           `(vector-set! ,var 0 ,val)
           `(set! ,var ,val))))
    ((eq? (car expr) 'begin)
     `(begin ,@(map (lambda (e) (box-transform e mutated-vars bound-vars)) (cdr expr))))
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr))
           (body (cddr expr)))
       (let* ((flat-params (if (list? params) params 
                               (let loop ((p params)) (if (pair? p) (cons (car p) (loop (cdr p))) (list p)))))
              (new-bound (append flat-params bound-vars))
              (params-to-box (let loop ((p flat-params))
                               (if (null? p) '()
                                   (if (memq (car p) mutated-vars)
                                       (cons (car p) (loop (cdr p)))
                                       (loop (cdr p)))))))
         `(lambda ,params
            ,(if (null? params-to-box)
                 (let ((new-body (map (lambda (e) (box-transform e mutated-vars new-bound)) body)))
                   (if (= (length new-body) 1) (car new-body) `(begin ,@new-body)))
                 `(let ,(map (lambda (v) `(,v (vector ,v))) params-to-box)
                    ,@(map (lambda (e) (box-transform e mutated-vars new-bound)) body)))))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (let* ((vars (map car bindings))
              (vals (map (lambda (v) (box-transform v mutated-vars bound-vars)) (map cadr bindings)))
              (new-bound (append vars bound-vars))
              (vars-to-box (let loop ((v vars))
                             (if (null? v) '()
                                 (if (memq (car v) mutated-vars)
                                     (cons (car v) (loop (cdr v)))
                                     (loop (cdr v)))))))
         `(let ,(map list vars vals)
            ,@(if (null? vars-to-box)
                  (map (lambda (e) (box-transform e mutated-vars new-bound)) body)
                  `((let ,(map (lambda (v) `(,v (vector ,v))) vars-to-box)
                      ,@(map (lambda (e) (box-transform e mutated-vars new-bound)) body))))))))
    ((eq? (car expr) 'define)
     `(define ,(cadr expr) ,(box-transform (caddr expr) mutated-vars bound-vars)))
    (else
     (map (lambda (e) (box-transform e mutated-vars bound-vars)) expr))))

;; --- Compiler ---

(define (make-context env free-vars next-reg)
  (vector env free-vars next-reg))

(define (ctx-env ctx) (vector-ref ctx 0))
(define (ctx-free-vars ctx) (vector-ref ctx 1))
(define (ctx-next-reg ctx) (vector-ref ctx 2))

(define (ctx-extend ctx var reg)
  (make-context (cons (cons var reg) (ctx-env ctx))
                (ctx-free-vars ctx)
                (+ (ctx-next-reg ctx) 1)))

(define (ctx-extend-n ctx bindings)
  (let loop ((bindings bindings) (c ctx))
    (if (null? bindings) c
        (let ((b (car bindings)))
          (loop (cdr bindings)
                (make-context (cons b (ctx-env c)) ;; Cons directly since b is (var . reg)
                              (ctx-free-vars c)
                              (max (+ (cdr b) 1) (ctx-next-reg c))))))))

(define (lookup-reg ctx var)
  (let ((pair (assq var (ctx-env ctx))))
    (if pair (cdr pair) #f)))

(define (lookup-free ctx var)
  (let ((vars (ctx-free-vars ctx)))
    (let loop ((vars vars) (idx 0))
      (cond ((null? vars) #f)
            ((eq? (car vars) var) idx)
            (else (loop (cdr vars) (+ idx 1)))))))

(define (compile-expr expr ctx target-reg code-acc tail?)
  (cond
   ((symbol? expr)
    (let ((acc (let ((reg (lookup-reg ctx expr)))
                 (if reg
                     (emit `(mov ,target-reg ,reg) code-acc)
                     (let ((free-idx (lookup-free ctx expr)))
                       (if free-idx
                           (emit `(closure-ref ,target-reg ,free-idx) code-acc)
                           (emit `(gref ,target-reg ,expr) code-acc)))))))
      (if tail? (emit `(return ,target-reg) acc) acc)))

   ((not (pair? expr)) ;; Constant
    (let ((acc (emit `(const ,target-reg ,expr) code-acc)))
      (if tail? (emit `(return ,target-reg) acc) acc)))

   ((eq? (car expr) 'quote)
    (let ((acc (emit `(const ,target-reg ,(cadr expr)) code-acc)))
       (if tail? (emit `(return ,target-reg) acc) acc)))

   ((eq? (car expr) 'begin)
     (compile-seq (cdr expr) ctx target-reg code-acc tail?))

   ((eq? (car expr) 'if)
    (let ((test-reg (ctx-next-reg ctx)))
      (let* ((ctx1 (make-context (ctx-env ctx) (ctx-free-vars ctx) (+ test-reg 1)))
             (acc1 (compile-expr (cadr expr) ctx1 test-reg code-acc #f))
             (true-label (gensym "L_true_"))
             (false-label (gensym "L_false_"))
             (end-label (gensym "L_end_")))
        (let* ((acc2 (emit `(if ,test-reg ,true-label ,false-label) acc1))
               (acc3 (emit-label true-label acc2))
               (acc4 (compile-expr (caddr expr) ctx target-reg acc3 tail?))
               (acc5 (if tail? acc4 (emit `(jump ,end-label) acc4))) ;; If tail, it returned.
               (acc6 (emit-label false-label acc5))
               (acc7 (if (null? (cdddr expr))
                         (let ((acc-f (emit `(const ,target-reg #f) acc6)))
                           (if tail? (emit `(return ,target-reg) acc-f) acc-f))
                         (compile-expr (cadddr expr) ctx target-reg acc6 tail?)))
               (acc8 (emit-label end-label acc7)))
          acc8))))

   ((eq? (car expr) 'set!)
    (let ((var (cadr expr))
          (val (caddr expr)))
      (let ((reg (ctx-next-reg ctx)))
        (let* ((ctx1 (make-context (ctx-env ctx) (ctx-free-vars ctx) (+ reg 1)))
               (acc1 (compile-expr val ctx1 reg code-acc #f))
               (local-reg (lookup-reg ctx var)))
          (let ((acc2 (if local-reg
                          (emit `(mov ,local-reg ,reg) acc1)
                          (let ((free-idx (lookup-free ctx var)))
                            (if free-idx
                                (let ((acc2 (emit `(closure-set! ,free-idx ,reg) acc1)))
                                  (emit `(const ,target-reg #f) acc2))
                                (emit `(gset ,var ,reg) acc1))))))
            (let ((acc3 (emit `(const ,target-reg #f) acc2)))
               (if tail? (emit `(return ,target-reg) acc3) acc3)))))))
   
   ((eq? (car expr) 'define)
    (let ((var (cadr expr))
          (val (caddr expr)))
      (let* ((reg (ctx-next-reg ctx))
             (ctx1 (make-context (ctx-env ctx) (ctx-free-vars ctx) (+ reg 1)))
             (acc1 (compile-expr val ctx1 reg code-acc #f)))
        (let* ((acc2 (emit `(gset ,var ,reg) acc1))
               (acc3 (emit `(const ,target-reg ,var) acc2)))
           (if tail? (emit `(return ,target-reg) acc3) acc3)))))

   ((eq? (car expr) 'lambda)
    (let* ((params (cadr expr))
           (body (cddr expr))
           (bound-vars (if (list? params) params (list params))) ;; Simplify rest args for now
           (current-bound (map car (ctx-env ctx)))
           (free-vars (analyze-free-vars expr current-bound)))
      
      ;; Compile body
      (let* ((new-env (map cons bound-vars (iota (length bound-vars)))) ;; Args start at reg 0
             (body-ctx (make-context new-env free-vars (length bound-vars)))
             ;; For begin, use loop.
             (body-code (compile-body body body-ctx))
             (closure-label (gensym "closure_code_")))
        
        (let ((free-regs (map (lambda (v)
                                (or (lookup-reg ctx v)
                                    (let ((idx (lookup-free ctx v)))
                                      (if idx `(closure-ref-marker ,idx) (error "Unbound variable" v))))) ;; Should handle closure-ref of parent
                              free-vars)))
             
             (let loop ((fvs free-vars)
                        (acc code-acc)
                        (collected-regs '())
                        (next-r (ctx-next-reg ctx)))
               (if (null? fvs)
                   (let ((acc1 (emit `(make-closure ,target-reg ,body-code ,@(reverse collected-regs)) acc)))
                     (if tail? (emit `(return ,target-reg) acc1) acc1))
                   (let* ((v (car fvs))
                          (reg (lookup-reg ctx v)))
                     (if reg
                         (loop (cdr fvs) acc (cons reg collected-regs) next-r)
                         (let ((f-idx (lookup-free ctx v)))
                           (if f-idx
                               (let* ((tmp-reg next-r)
                                      (acc1 (emit `(closure-ref ,tmp-reg ,f-idx) acc)))
                                 (loop (cdr fvs) acc1 (cons tmp-reg collected-regs) (+ next-r 1)))
                               (error "Unknown free variable (maybe global?)" v)))))))))))

   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr))
          (body (cddr expr)))
      (let* ((vars (map car bindings))
             (vals (map cadr bindings))
             (count (length vars))
             (start-reg (ctx-next-reg ctx)))
        ;; Compile init values
        (let loop-init ((vs vals)
                        (i 0)
                        (acc code-acc))
          (if (null? vs)
              ;; Body
              (let* ((ctx1 (ctx-extend-n ctx (map cons vars (iota count start-reg))))
                     (acc-body (compile-seq body ctx1 target-reg acc tail?)))
                 acc-body)
              (let ((reg (+ start-reg i)))
                ;; Use context with next-reg adjusted to avoid overwriting previous inits
                (let ((ctx-init (make-context (ctx-env ctx) (ctx-free-vars ctx) (+ reg 1))))
                  (loop-init (cdr vs) (+ i 1)
                             (compile-expr (car vs) ctx-init reg acc #f)))))))))
   
   (else
    ;; Application
    (let ((proc (car expr))
          (args (cdr expr)))
      (let ((proc-reg (ctx-next-reg ctx)))
        (let ((acc1 (compile-expr proc ctx proc-reg code-acc #f)))
          (let loop-args ((args args)
                          (i 0)
                          (acc acc1))
            (if (null? args)
                (if tail?
                    (emit `(tail-call ,proc-reg ,i) acc)
                    (emit `(call ,target-reg ,proc-reg ,i) acc))
                (let ((arg-reg (+ proc-reg 1 i)))
                   (let ((ctx1 (make-context (ctx-env ctx) (ctx-free-vars ctx) (+ arg-reg 1))))
                     (loop-args (cdr args) (+ i 1)
                                (compile-expr (car args) ctx1 arg-reg acc #f))))))))))
   ))

(define (emit inst acc)
  (cons inst acc))

(define (emit-label label acc)
  (cons label acc))

(define (compile-seq exprs ctx target-reg acc tail?)
  (if (null? (cdr exprs))
      (compile-expr (car exprs) ctx target-reg acc tail?)
      (let ((acc1 (compile-expr (car exprs) ctx target-reg acc #f)))
        (compile-seq (cdr exprs) ctx target-reg acc1 tail?))))

(define (compile-body body ctx)
  (let ((res-reg (ctx-next-reg ctx))) ;; Use a fresh register for the body's result
    (let ((acc (compile-seq body ctx res-reg '() #t))) ;; Tail context for body
      (reverse acc))))

(define (compile expr)
  (let ((mutated (analyze-mutated-vars expr)))
    (let ((boxed-expr (box-transform expr mutated '())))
      (let ((ctx (make-context '() '() 1))) ;; Reserve reg 0 for result
        (let ((acc (compile-expr boxed-expr ctx 0 '() #f))) ;; Top level is NOT tail context for now unless we wrap in lambda?
          ;; Usually top level just returns.
          (reverse (cons '(return 0) acc)))))))
