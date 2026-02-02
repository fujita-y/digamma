
;; --- Instruction Sets ---
(define cp:ops-loads '(const mov global-ref closure-ref closure-cell-ref reg-cell-ref))
(define cp:ops-memory-pairs '((global-set! . global-ref) (reg-cell-set! . reg-cell-ref) (closure-cell-set! . closure-cell-ref) (closure-set! . closure-ref)))

;; --- Instruction Accessors ---
(define (cp:inst-op inst) (car inst))
(define (cp:inst-arg1 inst) (cadr inst))
(define (cp:inst-arg2 inst) (caddr inst))

;; --- Utility: Set Operations ---
(define (cp:set-union s1 s2)
  (if (null? s1) s2
      (let ((rest (cp:set-union (cdr s1) s2)))
        (if (memq (car s1) s2) rest (cons (car s1) rest)))))

(define (cp:set-minus s1 s2)
  (if (null? s1) '()
      (let ((rest (cp:set-minus (cdr s1) s2)))
        (if (memq (car s1) s2) rest (cons (car s1) rest)))))

;; --- Utility: Parameter Analysis ---
(define (cp:flatten-params params)
  (cond
   ((null? params) '())
   ((symbol? params) (list params))
   ((pair? params) (cons (car params) (cp:flatten-params (cdr params))))
   (else '())))

(define (cp:analyze-params params)
  (let loop ((p params) (fixed '()))
    (cond
     ((null? p) (values (reverse fixed) #f #f))
     ((symbol? p) (values (reverse fixed) p #t))
     ((pair? p) (loop (cdr p) (cons (car p) fixed)))
     (else (error "Invalid parameter list" params)))))

;; --- Analysis 1: Free Variables ---
(define (cp:analyze-free-vars expr bound-vars)
  (cond
    ((symbol? expr) (if (memq expr bound-vars) '() (list expr)))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'if)
     (cp:set-union (cp:analyze-free-vars (cadr expr) bound-vars)
                (cp:set-union (cp:analyze-free-vars (caddr expr) bound-vars)
                           (if (null? (cdddr expr)) '() (cp:analyze-free-vars (cadddr expr) bound-vars)))))
    ((eq? (car expr) 'begin)
     (let loop ((exprs (cdr expr)))
       (if (null? exprs) '() (cp:set-union (cp:analyze-free-vars (car exprs) bound-vars) (loop (cdr exprs))))))
    ((eq? (car expr) 'set!)
     (let ((var (cadr expr)) (val (caddr expr)))
       (cp:set-union (if (memq var bound-vars) '() (list var)) (cp:analyze-free-vars val bound-vars))))
    ((eq? (car expr) 'define)
     (let ((head (cadr expr)))
       (if (pair? head)
           (cp:analyze-free-vars `(lambda ,(cdr head) ,@(cddr expr)) (cons (car head) bound-vars))
           (cp:analyze-free-vars (caddr expr) (cons head bound-vars)))))
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr)) (body (cddr expr)))
       (let ((new-bound (cp:flatten-params params)))
         (let ((body-free (let loop ((exprs body))
                            (if (null? exprs) '() (cp:set-union (cp:analyze-free-vars (car exprs) (append new-bound bound-vars)) (loop (cdr exprs)))))))
           (cp:set-minus body-free (append new-bound bound-vars))))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr)) (body (cddr expr)))
       (let ((vars (map car bindings)) (vals (map cadr bindings)))
         (cp:set-union (let loop ((vs vals)) (if (null? vs) '() (cp:set-union (cp:analyze-free-vars (car vs) bound-vars) (loop (cdr vs)))))
                    (let ((body-free (let loop ((exprs body))
                                       (if (null? exprs) '() (cp:set-union (cp:analyze-free-vars (car exprs) (append vars bound-vars)) (loop (cdr exprs)))))))
                      (cp:set-minus body-free (append vars bound-vars)))))))
    (else (let loop ((exprs expr)) (if (null? exprs) '() (cp:set-union (cp:analyze-free-vars (car exprs) bound-vars) (loop (cdr exprs))))))))

;; --- Check Recursive Pattern ---
(define (cp:match-rec-pattern expr)
  (and (pair? expr)
       (eq? (car expr) 'let)
       (let ((bindings (cadr expr))
             (body (cddr expr)))
         (and (pair? bindings)
              (null? (cdr bindings))
              (let ((name (car (car bindings))))
                (and (symbol? name)
                     (pair? body)
                     (pair? (car body))
                     (eq? (car (car body)) 'set!)
                     (eq? (cadr (car body)) name)
                     (pair? (caddr (car body)))
                     (eq? (car (caddr (car body))) 'lambda)))))))

;; --- Analysis 2: Mutated Variables ---
(define (cp:analyze-mutated-vars expr rec-vars)
  (cond
    ((pair? expr)
     (case (car expr)
       ((quote) '())
       ((set!) (if (memq (cadr expr) rec-vars)
                   (cp:analyze-mutated-vars (caddr expr) rec-vars)
                   (cons (cadr expr) (cp:analyze-mutated-vars (caddr expr) rec-vars))))
       ((lambda) (let loop ((body (cddr expr))) (if (null? body) '() (cp:set-union (cp:analyze-mutated-vars (car body) rec-vars) (loop (cdr body))))))
       (else (let loop ((exprs expr)) (if (null? exprs) '() (cp:set-union (cp:analyze-mutated-vars (car exprs) rec-vars) (loop (cdr exprs))))))))
    (else '())))

;; --- Analysis 3: Escape Analysis ---
(define (cp:var-escapes? var expr)
  (cond
   ((symbol? expr) (eq? expr var))
   ((not (pair? expr)) #f)
   ((eq? (car expr) 'quote) #f)
   ((eq? (car expr) 'if)
    (or (cp:var-escapes? var (cadr expr))
        (cp:var-escapes? var (caddr expr))
        (if (null? (cdddr expr)) #f (cp:var-escapes? var (cadddr expr)))))
   ((eq? (car expr) 'begin)
    (any (lambda (e) (cp:var-escapes? var e)) (cdr expr)))
   ((eq? (car expr) 'set!)
    (or (eq? (cadr expr) var) ;; If set! target is var, it's mutation, but checking if it escapes? Usually we care if it's used as value.
                              ;; But here we check if 'var' (the closure) is put into something.
                              ;; The var is the name of the closure. If we set! it, we are assigning TO it.
                              ;; We care if we use the value OF var.
        (cp:var-escapes? var (caddr expr))))
   ((eq? (car expr) 'define)
    (let ((head (cadr expr)))
      (if (pair? head)
          (cp:var-escapes? var `(lambda ,(cdr head) ,@(cddr expr)))
          (cp:var-escapes? var (caddr expr)))))
   ((eq? (car expr) 'lambda)
    (let ((params (cadr expr)) (body (cddr expr)))
       ;; If var is shadowed, it doesn't escape from inside here (referring to different var)
      (if (or (and (list? params) (memq var params))
              (and (symbol? params) (eq? var params)))
          #f
          (any (lambda (e) (cp:var-escapes? var e)) body))))
   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr)) (body (cddr expr)))
      (let ((vars (map car bindings)) (vals (map cadr bindings)))
        (if (memq var vars)
            (any (lambda (v) (cp:var-escapes? var v)) vals) ;; Shadowed in body, check init vals
            (or (any (lambda (v) (cp:var-escapes? var v)) vals)
                (any (lambda (e) (cp:var-escapes? var e)) body))))))
   (else
    ;; Function application (op args...)
    (let ((op (car expr))
          (args (cdr expr)))
      (cond
       ;; If it is in operator position (var ...), it is called, safe if only called.
       ((eq? op var)
        (any (lambda (arg) (cp:var-escapes? var arg)) args))
       (else
        ;; If it is in argument position, it escapes.
        (or (cp:var-escapes? var op)
            (any (lambda (arg) (cp:var-escapes? var arg)) args))))))))


;; --- Optimization: Peephole ---

(define (cp:optimize-two-inst code)
  (if (null? (cdr code)) #f
      (let ((i1 (car code)) (i2 (cadr code)))
        (cond
         ;; (mov A B) (mov B A) -> (mov A B)
         ((and (pair? i1) (eq? (cp:inst-op i1) 'mov)
               (pair? i2) (eq? (cp:inst-op i2) 'mov)
               (eq? (cp:inst-arg1 i1) (cp:inst-arg2 i2))
               (eq? (cp:inst-arg2 i1) (cp:inst-arg1 i2)))
          (cons i1 (cddr code)))
         
         ;; Check memory pairs
         ((let loop ((pairs cp:ops-memory-pairs))
            (if (null? pairs) #f
                (let* ((pair (car pairs)) (set-op (car pair)) (ref-op (cdr pair)))
                  (if (and (eq? (cp:inst-op i1) set-op) (eq? (cp:inst-op i2) ref-op))
                      (cond
                        ;; (set-op IDX B) (ref-op B IDX) -> (set-op IDX B)
                        ;; Generic check: arg1 matches arg2(ref) AND arg2 matches arg1(ref)
                       ((and (eq? (cp:inst-arg1 i1) (cp:inst-arg2 i2))
                             (eq? (cp:inst-arg2 i1) (cp:inst-arg1 i2)))
                        (cons i1 (cddr code)))

                        ;; (set-op IDX B) (ref-op C IDX) -> (set-op IDX B) (mov C B)
                       ((eq? (cp:inst-arg1 i1) (cp:inst-arg2 i2))
                        (cons i1 (cons `(mov ,(cp:inst-arg1 i2) ,(cp:inst-arg2 i1)) (cddr code))))
                       
                       (else (loop (cdr pairs))))
                      (loop (cdr pairs)))))))
         
         (else #f)))))

(define (cp:optimize-three-inst code)
  (if (null? (cddr code)) #f
      (let ((i1 (car code)) (i2 (cadr code)) (i3 (caddr code)))
        (if (and (pair? i1) (or (memq (cp:inst-op i1) cp:ops-loads) (assq (cp:inst-op i1) cp:ops-memory-pairs))
                 (pair? i2) (memq (cp:inst-op i2) cp:ops-loads)
                 (pair? i3) (or (memq (cp:inst-op i3) cp:ops-loads) (let ((p (assq (cp:inst-op i1) cp:ops-memory-pairs))) (and p (eq? (cp:inst-op i3) (cdr p))))))
            (let ((dst1 (cp:inst-arg1 i1)) (src1 (cp:inst-arg2 i1))
                  (dst2 (cp:inst-arg1 i2)) (src2 (cp:inst-arg2 i2))
                  (dst3 (cp:inst-arg1 i3)) (src3 (cp:inst-arg2 i3)))
              (cond
               ;; Pattern 1: Propagate Load into Move (if dead assignment)
               ;; (load A src) (mov C A) (clobber A ...) -> (load C src) (clobber A ...)
               ((and (memq (cp:inst-op i1) (delete 'mov cp:ops-loads))
                     (eq? dst1 src2)
                     (eq? dst1 dst3))
                (cons `(,(cp:inst-op i1) ,dst2 ,src1) (cons i3 (cdddr code))))
               
               ;; Pattern 2: Propagate Move into Move (if dead assignment)
               ;; (mov A B) (mov C A) (clobber A ...) -> (mov C B) (clobber A ...)
               ((and (eq? (cp:inst-op i1) 'mov)
                     (eq? dst1 src2)
                     (eq? dst1 dst3)
                     (not (eq? dst1 src3)))
                (cons `(mov ,dst2 ,src1) (cons i3 (cdddr code))))

               ;; Pattern 3: Redundant Restore
               ;; (mov rA rB) (op rC <data>) (mov rB rA) -> (mov rA rB) (op rC <data>)
               ;; Condition: rC != rA && rC != rB
               ((and (eq? (cp:inst-op i1) 'mov)
                     (memq (cp:inst-op i2) (delete 'mov cp:ops-loads))
                     (eq? (cp:inst-op i3) 'mov)
                     (eq? dst1 src3)       ; rA == rA in (mov rB rA)
                     (eq? src1 dst3)       ; rB == rB in (mov rB rA)
                     (not (eq? dst2 dst1)) ; rC != rA
                     (not (eq? dst2 src1))); rC != rB
                (cons i1 (cons i2 (cdddr code))))
               
               ;; Patterns 4, 5, 6: Generic Memory Set ... Memory Ref
               ;; (set-op IDX src) (op ...) (ref-op dst IDX) -> (set-op IDX src) (op ...) (mov dst src)
               ;; Checks for aliasing between (op ...) and the source/index registers.
               ((let ((pair (assq (cp:inst-op i1) cp:ops-memory-pairs)))
                  (and pair
                       (eq? (cp:inst-op i3) (cdr pair))
                       (eq? dst1 src3)       ; IDX matches IDX
                       (not (eq? dst2 src1)) ; Intermediate op doesn't clobber source
                       ;; Ensure intermediate op doesn't clobber the Index (if it's a register)
                       (or (not (symbol? dst1)) (not (eq? dst2 dst1)))))
                (cons i1 (cons i2 (cons `(mov ,dst3 ,src1) (cdddr code)))))

               (else #f)))
            #f))))

(define (cp:optimize-restore-chain code)
  (if (null? code) #f
      (let ((i1 (car code)))
        (if (and (pair? i1) (eq? (cp:inst-op i1) 'mov))
            (let ((rA (cp:inst-arg1 i1)) (rB (cp:inst-arg2 i1)))
              (let loop ((rest (cdr code)) (acc '()) (count 0))
                (if (null? rest) #f
                    (let ((next (car rest)))
                      (cond
                        ;; Found restore (mov rB rA)
                        ((and (eq? (cp:inst-op next) 'mov)
                              (eq? (cp:inst-arg1 next) rB)
                              (eq? (cp:inst-arg2 next) rA))
                         ;; Optimize: i1 + reversed-acc + (cdr rest), excluding 'next'
                         (cons i1 (append (reverse acc) (cdr rest))))
                        
                        ;; Check if 'next' is a safe load instruction that clobbers neither rA nor rB
                        ((and (memq (cp:inst-op next) cp:ops-loads)
                              (not (eq? (cp:inst-arg1 next) rA))
                              (not (eq? (cp:inst-arg1 next) rB)))
                         (loop (cdr rest) (cons next acc) (+ count 1)))
                        
                        ;; Otherwise, clobbered or unsafe op
                        (else #f))))))
            #f))))

(define (cp:peephole-optimize-pass code)
  (cond
    ((null? code) '())
    ((null? (cdr code)) 
     ;; Check last instruction for self-move
     (let ((inst (car code)))
       (if (and (pair? inst) (eq? (cp:inst-op inst) 'mov) (eq? (cp:inst-arg1 inst) (cp:inst-arg2 inst)))
           '()
           code)))

    ;; (mov rA rA) -> removed
    ((and (pair? (car code)) (eq? (cp:inst-op (car code)) 'mov)
          (eq? (cp:inst-arg1 (car code)) (cp:inst-arg2 (car code))))
     (cp:peephole-optimize-pass (cdr code)))
    
    ;; Try 2-inst optimizations
    ((cp:optimize-two-inst code) => cp:peephole-optimize-pass)

    ;; Try 3-inst optimizations
    ((cp:optimize-three-inst code) => cp:peephole-optimize-pass)

    ;; Try N-inst restore chain optimization
    ((cp:optimize-restore-chain code) => cp:peephole-optimize-pass)

    (else (cons (car code) (cp:peephole-optimize-pass (cdr code))))))


(define (cp:peephole-optimize code)
  (let loop ((current code))
    (let ((next (cp:peephole-optimize-pass current)))
      (if (equal? current next)
          current
          (loop next)))))

;; --- Optimization: Analyze Max Outgoing Args ---
(define (cp:analyze-max-outgoing-args expr)
  (cond
   ((not (pair? expr)) 0)
   ((symbol? (car expr))
    (case (car expr)
      ((quote) 0)
      ((if) (max (cp:analyze-max-outgoing-args (cadr expr))
                 (cp:analyze-max-outgoing-args (caddr expr))
                 (if (null? (cdddr expr)) 0 (cp:analyze-max-outgoing-args (cadddr expr)))))
      ((begin) (apply max 0 (map cp:analyze-max-outgoing-args (cdr expr))))
      ((set!) (cp:analyze-max-outgoing-args (caddr expr)))
      ((lambda) 0) ;; New scope
      ((let)
       (let ((bindings (cadr expr)) (body (cddr expr)))
         (max (apply max 0 (map (lambda (b) (cp:analyze-max-outgoing-args (cadr b))) bindings))
              (apply max 0 (map cp:analyze-max-outgoing-args body)))))
      (else ;; Application
       (let ((args (cdr expr))
             (proc (car expr)))
         (max (length args)
              (cp:analyze-max-outgoing-args proc)
              (apply max 0 (map cp:analyze-max-outgoing-args args)))))))
   (else ;; Should not happen in this simplified scheme, but recurse just in case
    (apply max 0 (map cp:analyze-max-outgoing-args expr)))))

(define (cp:compile expr)
  (let* ((rec-vars '())
         (_ (let walk ((e expr))
              (if (cp:match-rec-pattern e) (set! rec-vars (cons (car (car (cadr e))) rec-vars)))
              (cond
               ((pair? e)
                (if (eq? (car e) 'quote)
                    '()
                    (begin (walk (car e)) (walk (cdr e)))))
               (else '()))))

         (mutated (cp:analyze-mutated-vars expr rec-vars))
         (ctx (cp:make-context mutated)))
    
    (cp:codegen expr '() (max 1 (cp:analyze-max-outgoing-args expr)) #f ctx)
    (cp:ctx-emit! ctx `(ret))
    
    (let* ((main-code (reverse (cp:ctx-code ctx)))
           (closure-code (apply append (cp:ctx-all-closures ctx)))
           (all-code (cp:peephole-optimize (append main-code closure-code)))
           (label-map (make-hash-table 'eq?))
           (final-code '()) (current-pc 0))

      (for-each (lambda (inst)
                  (if (eq? (car inst) 'label)
                      (hash-table-put! label-map (cadr inst) current-pc)
                      (set! current-pc (+ current-pc 1))))
                all-code)
      (for-each (lambda (inst)
                  (if (not (eq? (car inst) 'label))
                      (let ((resolved (map (lambda (x)
                                             (if (and (symbol? x) (hash-table-exists? label-map x))
                                                 (hash-table-get label-map x) x))
                                           inst)))
                        (set! final-code (cons (list->vector resolved) final-code)))))
                all-code)
      (list->vector (reverse final-code)))))

;; --- Compiler Context & Helpers ---
(define (cp:make-context mutated-vars) (vector '() 0 0 '() mutated-vars))
(define (cp:ctx-code ctx) (vector-ref ctx 0))
(define (cp:ctx-set-code! ctx code) (vector-set! ctx 0 code))
(define (cp:ctx-emit! ctx inst) (vector-set! ctx 0 (cons inst (vector-ref ctx 0))))
(define (cp:ctx-labels ctx) (vector-ref ctx 1))
(define (cp:ctx-inc-labels! ctx) (vector-set! ctx 1 (+ (vector-ref ctx 1) 1)) (vector-ref ctx 1))
(define (cp:ctx-closure-labels ctx) (vector-ref ctx 2))
(define (cp:ctx-inc-closure-labels! ctx) (vector-set! ctx 2 (+ (vector-ref ctx 2) 1)) (vector-ref ctx 2))
(define (cp:ctx-all-closures ctx) (vector-ref ctx 3))
(define (cp:ctx-add-closure! ctx code) (vector-set! ctx 3 (cons code (vector-ref ctx 3))))
(define (cp:ctx-mutated ctx) (vector-ref ctx 4))

(define (cp:gen-label ctx . type)
  (if (and (not (null? type)) (eq? (car type) 'closure))
      (string->symbol (string-append "C" (number->string (cp:ctx-inc-closure-labels! ctx))))
      (string->symbol (string-append "L" (number->string (cp:ctx-inc-labels! ctx))))))

(define (cp:make-reg i) (string->symbol (string-append "r" (number->string i))))

(define (cp:lookup var env)
  (let loop ((e env))
    (if (null? e) (cons 'global var)
        (let ((scope (car e)))
          (if (and (pair? scope) (eq? (car scope) 'cl))
              (let ((res (assoc var (cdr scope))))
                (if res (cons 'cl (cdr res)) (loop (cdr e))))
              (let ((res (assoc var scope)))
                (if res 
                    (if (eq? (cdr res) 'self)
                        (cons 'self #f)
                        (cons 'reg (cdr res)))
                    (loop (cdr e)))))))))


(define (cp:codegen-args args env arg-base ctx)
  (let loop ((as args) (i 0))
    (if (not (null? as))
        (let ((temp-reg (cp:make-reg (+ arg-base i))))
          (cp:codegen (car as) env (+ arg-base i 1) #f ctx)
          (cp:ctx-emit! ctx `(mov ,temp-reg r0))
          (loop (cdr as) (+ i 1))))))

(define (cp:move-args num-args base ctx)
  (let move-loop ((j 0))
    (if (< j num-args)
        (begin (cp:ctx-emit! ctx `(mov ,(cp:make-reg j) ,(cp:make-reg (+ base j)))) (move-loop (+ j 1))))))

(define (cp:codegen-symbol expr env tail? ctx)
  (let ((binding (cp:lookup expr env)))
    (cond
      ((eq? (car binding) 'reg) (if (memq expr (cp:ctx-mutated ctx)) (cp:ctx-emit! ctx `(reg-cell-ref r0 ,(cdr binding))) (cp:ctx-emit! ctx `(mov r0 ,(cdr binding)))))
      ((eq? (car binding) 'cl) (if (memq expr (cp:ctx-mutated ctx)) (cp:ctx-emit! ctx `(closure-cell-ref r0 ,(cdr binding))) (cp:ctx-emit! ctx `(closure-ref r0 ,(cdr binding)))))
      ((eq? (car binding) 'self) (cp:ctx-emit! ctx `(closure-self r0)))
      (else (cp:ctx-emit! ctx `(global-ref r0 ,expr))))
    (if tail? (cp:ctx-emit! ctx `(ret)))))


(define (cp:codegen-if expr env next-reg tail? ctx)
  (let ((t-label (cp:gen-label ctx)) (f-label (cp:gen-label ctx)) (end-label (cp:gen-label ctx)))
    (cp:codegen (cadr expr) env next-reg #f ctx)
    (cp:ctx-emit! ctx `(if ,t-label ,f-label))
    (cp:ctx-emit! ctx `(label ,t-label))
    (cp:codegen (caddr expr) env next-reg tail? ctx)
    (if (not tail?) (cp:ctx-emit! ctx `(jump ,end-label)))
    (cp:ctx-emit! ctx `(label ,f-label))
    (if (null? (cdddr expr))
        (begin (cp:ctx-emit! ctx `(const r0 #f)) (if tail? (cp:ctx-emit! ctx `(ret))))
        (cp:codegen (cadddr expr) env next-reg tail? ctx))
    (if (not tail?) (cp:ctx-emit! ctx `(label ,end-label)))))

(define (cp:codegen-set! expr env next-reg tail? ctx)
  (let ((var (cadr expr)) (val (caddr expr)))
    (cp:codegen val env next-reg #f ctx)
    (let ((binding (cp:lookup var env)))
      (cond
        ((eq? (car binding) 'reg) (if (memq var (cp:ctx-mutated ctx)) (cp:ctx-emit! ctx `(reg-cell-set! ,(cdr binding) r0)) (cp:ctx-emit! ctx `(mov ,(cdr binding) r0))))
        ((eq? (car binding) 'cl) (cp:ctx-emit! ctx `(closure-cell-set! ,(cdr binding) r0)))
        (else (cp:ctx-emit! ctx `(global-set! ,var r0))))
      (if tail? (cp:ctx-emit! ctx `(ret))))))

(define (cp:codegen-lambda expr env next-reg tail? ctx stack-alloc?)
  (let* ((params (cadr expr)) (body (cddr expr))
         (all-free (cp:analyze-free-vars expr '()))
         (params-list (cp:flatten-params params))
         (potential-free (cp:set-minus all-free params-list))
         (free (filter (lambda (f) (let ((b (cp:lookup f env))) (not (eq? (car b) 'global)))) potential-free))
         (entry-label (cp:gen-label ctx 'closure))
         (prev-code (cp:ctx-code ctx))
         (max-outgoing (apply max 0 (map cp:analyze-max-outgoing-args body))))
    
    (let-values (((fixed-params rest-param has-rest?) (cp:analyze-params params)))
       (let ((n-fixed (length fixed-params)))

         (cp:ctx-set-code! ctx '())
         (cp:ctx-emit! ctx `(label ,entry-label))

         ;; Arg mapping:
         ;; Fixed params 0..(n-fixed-1) are in r0..(rn-1)
         ;; Rest param is in r(n-fixed) (if has-rest?)
         ;; We need to move them to new-base..(new-base+n-vars-1)
         
         (let* ((new-base (max 1 max-outgoing)) ; Ensure space for outgoing args
                (all-params (if has-rest? (append fixed-params (list rest-param)) fixed-params))
                (n-total-params (length all-params))
                
                (new-scope (let loop ((p all-params) (i 0))
                             (if (null? p) '() (cons (cons (car p) (cp:make-reg (+ new-base i))) (loop (cdr p) (+ i 1))))))
                (cl-scope (let loop ((f free) (i 0))
                            (if (null? f) '() (cons (cons (car f) i) (loop (cdr f) (+ i 1)))))))
           
           ;; Move params.
           ;; Incoming: r0 .. r(n-total-params - 1)
           ;; Dest: new-base ..
           ;; Move via reverse order
           (let move-params ((i (- n-total-params 1)))
             (if (>= i 0) (begin (cp:ctx-emit! ctx `(mov ,(cp:make-reg (+ new-base i)) ,(cp:make-reg i))) (move-params (- i 1)))))

           (let* ((self-bindings (if (null? env) '()
                                     (let ((scope (car env)))
                                       (if (and (pair? scope) (not (eq? (car scope) 'cl)) (not (eq? (car scope) 'num-params)))
                                           (filter (lambda (b) (eq? (cdr b) 'self)) scope)
                                           '())))))
             (for-each (lambda (p) (if (memq (car p) (cp:ctx-mutated ctx)) (cp:ctx-emit! ctx `(make-cell ,(cdr p) ,(cdr p))))) new-scope)
             (cp:codegen `(begin ,@body) (cons (cons `num-params n-total-params) (cons new-scope (cons self-bindings (list (cons 'cl cl-scope))))) (+ new-base n-total-params) #t ctx)))

         (cp:ctx-add-closure! ctx (reverse (cp:ctx-code ctx)))
         (cp:ctx-set-code! ctx prev-code)

         (let loop ((fs free) (regs '()) (r next-reg))
           (if (null? fs)
               (begin
                 (cp:ctx-emit! ctx `(make-closure r0 ,entry-label ,(reverse regs) ,stack-alloc? ,n-fixed ,has-rest?))
                 (if tail? (cp:ctx-emit! ctx `(ret))))
               (let ((b (cp:lookup (car fs) env)))
                 (if (eq? (car b) 'reg)
                     (loop (cdr fs) (cons (cdr b) regs) r)
                     ;; It is 'cl (closure) or 'self
                     (let ((tmp (cp:make-reg r)))
                       (if (eq? (car b) 'self)
                           (cp:ctx-emit! ctx `(closure-self ,tmp))
                           (cp:ctx-emit! ctx `(closure-ref ,tmp ,(cdr b))))
                       (loop (cdr fs) (cons tmp regs) (+ r 1)))))))))))

(define (cp:codegen-application expr env next-reg tail? ctx)
  (if (and (pair? (car expr)) (cp:match-rec-pattern (car expr)))
      (let* ((let-expr (car expr)) (args (cdr expr)) (num-args (length args))
             (bindings (cadr let-expr)) (name (caar bindings)) (sym (cadar bindings))
             (body (cddr let-expr)) (set-expr (car body)) (lambda-expr (caddr set-expr))
             (reg (cp:make-reg next-reg)) (arg-base (+ next-reg 1)))
        (cp:ctx-emit! ctx `(const r0 ,sym)) (cp:ctx-emit! ctx `(mov ,reg r0))
        ;; For recursive let/match pattern, it's strictly a local binding structure by definition of match-rec-pattern (let ((name ...)) (set! name (lambda ...))) 
        ;; We can enable stack allocation if 'name' does not escape in 'body'.
        ;; The body starts with (set! name ...) so we check rest of body.
        (let ((escapes? (cp:var-escapes? name `(begin ,@(cdr body)))))
             (cp:codegen-lambda lambda-expr (cons (list (cons name 'self)) env) next-reg #f ctx (not escapes?)))
        (cp:ctx-emit! ctx `(mov ,reg r0))
        (cp:codegen-args args (cons (list (cons name reg)) env) arg-base ctx)
        (let ((call-reg (cp:make-reg (+ arg-base num-args))))
          (cp:ctx-emit! ctx `(mov ,call-reg ,reg))
          (cp:move-args num-args arg-base ctx)
          (if tail? (cp:ctx-emit! ctx `(tail-call ,call-reg ,num-args)) (cp:ctx-emit! ctx `(call ,call-reg ,num-args)))))

      (let* ((proc (car expr)) (args (cdr expr)) (num-args (length args))
             (base-reg (max 1 next-reg)))
        (cp:codegen-args args env base-reg ctx)
        (let ((call-reg (cp:make-reg (+ base-reg num-args))))
          ;; Anonymous lambda call: ((lambda ...) ...) -> Stack allocate!
          (if (and (pair? proc) (eq? (car proc) 'lambda))
              (cp:codegen-lambda proc env (+ base-reg num-args) #f ctx #t)
              (cp:codegen proc env (+ base-reg num-args) #f ctx))
          (cp:ctx-emit! ctx `(mov ,call-reg r0))
          (cp:move-args num-args base-reg ctx)
          (if tail? (cp:ctx-emit! ctx `(tail-call ,call-reg ,num-args)) (cp:ctx-emit! ctx `(call ,call-reg ,num-args)))))))

(define (cp:codegen-let expr env next-reg tail? ctx)
  (if (cp:match-rec-pattern expr)
      (let* ((bindings (cadr expr))
             (name (caar bindings))
             (sym (cadar bindings))
             (body (cddr expr))
             (reg (cp:make-reg next-reg)))
        (cp:ctx-emit! ctx `(const r0 ,sym))
        (cp:ctx-emit! ctx `(mov ,reg r0))
        (let ((set-expr (car body))
              (rest (cdr body)))
          (let ((lambda-expr (caddr set-expr)))
            ;; Recursive pattern again
            (let ((escapes? (cp:var-escapes? name `(begin ,@(cdr body)))))
                 (cp:codegen-lambda lambda-expr (cons (list (cons name 'self)) env) (+ next-reg 1) #f ctx (not escapes?))))
          (cp:ctx-emit! ctx `(mov ,reg r0))
          (cp:codegen `(begin ,@rest) (cons (list (cons name reg)) env) (+ next-reg 1) tail? ctx)))
      (let* ((bindings (cadr expr)) (body (cddr expr)) (vars (map car bindings)) (vals (map cadr bindings)))
        (let loop ((vs vals) (vars vars) (r next-reg) (new-scope '()))
          (if (null? vs) (cp:codegen `(begin ,@body) (cons new-scope env) r tail? ctx)
              (let ((reg (cp:make-reg r)))
                (if (and (pair? (car vs)) (eq? (car (car vs)) 'lambda))
                    (let ((escapes? (cp:var-escapes? (car vars) `(begin ,@body))))
                      (cp:codegen-lambda (car vs) env (+ r 1) #f ctx (not escapes?)))
                    (cp:codegen (car vs) env (+ r 1) #f ctx))
                (cp:ctx-emit! ctx `(mov ,reg r0))
                (if (memq (car vars) (cp:ctx-mutated ctx)) (cp:ctx-emit! ctx `(make-cell ,reg ,reg)))
                (loop (cdr vs) (cdr vars) (+ r 1) (cons (cons (car vars) reg) new-scope))))))))

(define (cp:codegen expr env next-reg tail? ctx)
  (cond
    ((symbol? expr) (cp:codegen-symbol expr env tail? ctx))
    ((not (pair? expr)) (cp:ctx-emit! ctx `(const r0 ,expr)) (if tail? (cp:ctx-emit! ctx `(ret))))
    ((eq? (car expr) 'quote) (cp:ctx-emit! ctx `(const r0 ,(cadr expr))) (if tail? (cp:ctx-emit! ctx `(ret))))
    ((eq? (car expr) 'begin)
     (let loop ((exprs (cdr expr)))
       (if (null? (cdr exprs)) (cp:codegen (car exprs) env next-reg tail? ctx)
           (begin (cp:codegen (car exprs) env next-reg #f ctx) (loop (cdr exprs))))))
    ((eq? (car expr) 'define)
     (let ((var (cadr expr)) (val (caddr expr)))
       (cp:codegen val env next-reg #f ctx) (cp:ctx-emit! ctx `(global-set! ,var r0)) (cp:ctx-emit! ctx `(global-ref r0 ,var)) (if tail? (cp:ctx-emit! ctx `(ret)))))
    ((eq? (car expr) 'if) (cp:codegen-if expr env next-reg tail? ctx))
    ((eq? (car expr) 'set!) (cp:codegen-set! expr env next-reg tail? ctx))
    ((eq? (car expr) 'lambda) (cp:codegen-lambda expr env next-reg tail? ctx #f)) ; Default to #f if raw lambda
    ((eq? (car expr) 'let) (cp:codegen-let expr env next-reg tail? ctx))
    (else (cp:codegen-application expr env next-reg tail? ctx))))
