
;;; Optimizer for Scheme Core Forms
;;; Core Forms: [lambda, let, define, if, set!, quote, begin] and applications.

;; --- Global State ---

(define op:global-env (make-hash-table 'eq?))
(define op:*inlining-depth* (make-hash-table 'eq?))

;; --- Core API ---

(define (op:optimize expr)
  (hash-table-clear! op:global-env)
  (hash-table-clear! op:*inlining-depth*)
  (let loop ((current expr) (prev '()) (iters 0))
    (if (or (equal? current prev) (>= iters 10))
        current
        (loop (op:optimize-once-inner current) current (+ iters 1)))))

(define (op:optimize-once expr)
  (op:optimize-once-inner expr))

(define (op:boolean-true? x) (or (eq? x #t) (equal? x ''#t)))
(define (op:boolean-false? x) (or (eq? x #f) (equal? x ''#f) (null? x)))

;; --- Dispatcher ---

(define (op:optimize-once-inner expr)
  (cond
    ((symbol? expr) 
     (if (hash-table-exists? op:global-env expr)
         (hash-table-get op:global-env expr)
         expr))
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'if) (op:opt-if expr))
    ((eq? (car expr) 'begin) (op:opt-begin expr))
    ((eq? (car expr) 'lambda) (op:opt-lambda expr))
    ((eq? (car expr) 'let) (op:opt-let expr))
    ((eq? (car expr) 'set!)
     `(set! ,(cadr expr) ,(op:optimize-once-inner (caddr expr))))
    ((eq? (car expr) 'define)
     (let ((var (cadr expr)) (val (op:optimize-once-inner (caddr expr))))
       (if (and (not (pair? val)) (not (symbol? val))) ;; Simple constant
           (hash-table-put! op:global-env var val))
       (if (and (pair? val) (eq? (car val) 'quote))
           (hash-table-put! op:global-env var val))
       `(define ,var ,val)))
    (else (op:opt-app expr))))

;; --- Specialized Optimizers ---

(define (op:opt-if expr)
  (let ((test (op:optimize-once-inner (cadr expr)))
        (then (op:optimize-once-inner (caddr expr)))
        (else (if (null? (cdddr expr)) ''#f (op:optimize-once-inner (cadddr expr)))))
    (cond
      ;; If-lifting: (if (if a b c) d e) -> (if a (if b d e) (if c d e))
      ((and (pair? test) (eq? (car test) 'if))
       (let ((a (cadr test)) (b (caddr test)) (c (cadddr test)))
         (op:optimize-once-inner `(if ,a (if ,b ,then ,else) (if ,c ,then ,else)))))
      ;; Boolean simplification: (if a #t #f) -> a (if a is boolean)
      ((and (op:boolean-true? then) (op:boolean-false? else))
       test)
      ((and (pair? test) (eq? (car test) 'quote))
       (if (cadr test) then else))
      ((boolean? test)
       (if test then else))
      (else `(if ,test ,then ,else)))))

(define (op:opt-begin expr)
  (let* ((exprs (map op:optimize-once-inner (cdr expr)))
         (flattened (apply append (map (lambda (x) (if (and (pair? x) (eq? (car x) 'begin)) (cdr x) (list x))) exprs)))
         (filtered (filter (lambda (x) (op:has-effects? x)) (op:take flattened (- (length flattened) 1)))))
    (let ((last-val (car (reverse flattened))))
      (if (null? filtered)
          last-val
          `(begin ,@filtered ,last-val)))))

(define (op:opt-lambda expr)
  (let* ((params (cadr expr))
         (body-exprs (map op:optimize-once-inner (cddr expr)))
         (body `(begin ,@body-exprs))
         (used (op:analyze-used-vars body))
         (mutated (op:analyze-mutated-vars body)))
    ;; Unused parameter removal
    (let loop ((ps (if (list? params) params '())) (i 0) (new-params '()))
      (cond
        ((null? ps) `(lambda ,(if (list? params) (reverse new-params) params) ,@body-exprs))
        ((and (not (memq (car ps) used)) (not (memq (car ps) mutated)))
         (loop (cdr ps) (+ i 1) new-params)) ;; Remove it
        (else (loop (cdr ps) (+ i 1) (cons (car ps) new-params)))))))

(define (op:opt-let expr)
  (let* ((bindings (map (lambda (b) (list (car b) (op:optimize-once-inner (cadr b)))) (cadr expr)))
         (body-exprs (map op:optimize-once-inner (cddr expr)))
         (body (if (and (null? (cdr body-exprs)) (pair? (car body-exprs)) (eq? (car (car body-exprs)) 'begin))
                   (cdr (car body-exprs))
                   body-exprs)))
    
    ;; Let-floating
    (let ((floated-bindings '()) (main-bindings '()))
      (for-each (lambda (b)
                  (let ((var (car b)) (val (cadr b)))
                    (if (and (pair? val) (eq? (car val) 'let))
                        (begin
                          (set! floated-bindings (append floated-bindings (cadr val)))
                          (set! main-bindings (append main-bindings (list (list var (if (null? (cddr val)) '() (car (reverse (cddr val)))))))))
                        (set! main-bindings (append main-bindings (list b))))))
                bindings)
      (if (not (null? floated-bindings))
          (op:optimize-once-inner `(let ,floated-bindings (let ,main-bindings ,@body)))
          
          ;; Binding Optimization Pipeline
          (let* ((used (op:analyze-used-vars `(begin ,@body)))
                 (mutated-vars (op:analyze-mutated-vars `(begin ,@body)))
                 (new-bindings '()))
            
            (define (process-binding! b)
              (let ((var (car b)) (val (cadr b)))
                (cond
                  ;; 1. Copy Propagation
                  ((and (not (memq var mutated-vars)) (or (not (pair? val)) (and (pair? val) (eq? (car val) 'quote)) (symbol? val)))
                   (set! body (map (lambda (e) (op:substitute e var val)) body))
                   (if (op:has-effects? val) (set! new-bindings (cons b new-bindings))))
                  
                  ;; 2. Inlining
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (op:should-inline? var val `(begin ,@body)))
                   (set! body (op:perform-inlining var val body))
                   (if (op:has-effects? val) (set! new-bindings (cons b new-bindings))))
                  
                  ;; 3. Lambda Dropping
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (and (pair? (car body)) (eq? (car (car body)) 'if)))
                   (let ((res (op:try-drop-lambda var val (car body))))
                     (if (car res)
                         (set! body (list (cdr res)))
                         (set! new-bindings (cons b new-bindings)))))
                  
                  ;; 4. Lambda Lifting (Handled by Let-floating in next pass)
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (null? (op:analyze-free-vars val '())))
                   (set! new-bindings (cons b new-bindings)))
                  
                  ;; 5. Keep it if used or has effects
                  ((or (memq var used) (memq var mutated-vars) (op:has-effects? val))
                   (set! new-bindings (cons b new-bindings))))))

            (for-each process-binding! main-bindings)
            (set! new-bindings (reverse new-bindings))
            
            (if (null? new-bindings)
                (if (null? (cdr body)) (car body) `(begin ,@body))
                `(let ,new-bindings ,@body)))))))

(define (op:try-drop-lambda var val if-expr)
  (let* ((test (cadr if-expr))
         (then (caddr if-expr))
         (else (if (null? (cdddr if-expr)) ''#f (cadddr if-expr)))
         (used-in-test (memq var (op:analyze-used-vars test)))
         (used-in-then (memq var (op:analyze-used-vars then)))
         (used-in-else (memq var (op:analyze-used-vars else))))
    (cond
      ((and (not used-in-test) used-in-then (not used-in-else))
       (cons #t `(if ,test (let ((,var ,val)) ,then) ,else)))
      ((and (not used-in-test) (not used-in-then) used-in-else)
       (cons #t `(if ,test ,then (let ((,var ,val)) ,else))))
      (else (cons #f #f)))))

(define (op:opt-app expr)
  (let ((proc (op:optimize-once-inner (car expr)))
        (args (map op:optimize-once-inner (cdr expr))))
    (cond
      ;; Beta-reduction: ((lambda (x) ...) y) -> (let ((x y)) ...)
      ((and (pair? proc) (eq? (car proc) 'lambda))
       (let ((params (cadr proc))
             (body (cddr proc)))
         (if (= (length params) (length args))
             (op:optimize-once `(let ,(map list params args) ,@body))
             ;; Handle argument mismatch
             (if (< (length params) (length args))
                 (let ((extra-args (op:drop args (length params))))
                   (op:optimize-once `(begin ,@extra-args (let ,(map list params (op:take args (length params))) ,@body))))
                 (cons proc args)))))
      (else (cons proc args)))))

;; --- Transformation Helpers ---

(define (op:take lst n)
  (if (or (null? lst) (<= n 0)) '()
      (cons (car lst) (op:take (cdr lst) (- n 1)))))

(define (op:drop lst n)
  (if (or (null? lst) (<= n 0)) lst
      (op:drop (cdr lst) (- n 1))))

(define (op:substitute expr var val)
  (cond
    ((eq? expr var) val)
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         expr
         `(lambda ,(cadr expr) ,@(map (lambda (e) (op:substitute e var val)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (if (memq var vars)
           `(let ,(map (lambda (b) (list (car b) (op:substitute (cadr b) var val))) (cadr expr)) ,@(cddr expr))
           `(let ,(map (lambda (b) (list (car b) (op:substitute (cadr b) var val))) (cadr expr)) ,@(map (lambda (e) (op:substitute e var val)) (cddr expr))))))
    (else (map (lambda (e) (op:substitute e var val)) expr))))

(define (op:substitute-proc expr var val)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         expr
         `(lambda ,(cadr expr) ,@(map (lambda (e) (op:substitute-proc e var val)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (if (memq var vars)
           `(let ,(map (lambda (b) (list (car b) (op:substitute-proc (cadr b) var val))) (cadr expr)) ,@(cddr expr))
           `(let ,(map (lambda (b) (list (car b) (op:substitute-proc (cadr b) var val))) (cadr expr)) ,@(map (lambda (e) (op:substitute-proc e var val)) (cddr expr))))))
    ((eq? (car expr) var) 
     (cons val (map (lambda (e) (op:substitute-proc e var val)) (cdr expr))))
    (else (let ((new-expr (map (lambda (e) (op:substitute-proc e var val)) expr)))
            (if (and (pair? (car new-expr)) (eq? (car (car new-expr)) 'lambda))
                (op:optimize-once new-expr) ;; Optimization opportunity (beta-reduction)
                new-expr)))))

(define (op:perform-inlining var val body)
  (let ((depth (hash-table-get op:*inlining-depth* var 0)))
    (hash-table-put! op:*inlining-depth* var (+ depth 1))
    (let ((new-body (map (lambda (e) (op:substitute-proc e var val)) body)))
      new-body)))

;; --- Inlining Heuristics ---

(define op:*cp0-effort-limit* 100)
(define op:*cp0-score-limit* 20)

(define (op:compute-score expr effort)
  (if (<= effort 0) 1000 ;; Too much effort
      (cond
        ((not (pair? expr)) 1)
        ((eq? (car expr) 'quote) 1)
        ((eq? (car expr) 'if)
         (+ 1 (op:compute-score (cadr expr) (- effort 1))
            (max (op:compute-score (caddr expr) (- effort 1))
                 (op:compute-score (cadddr expr) (- effort 1)))))
        ((eq? (car expr) 'lambda)
         (+ 5 (op:compute-score (cddr expr) (- effort 1))))
        (else (+ 1 (apply + (map (lambda (e) (op:compute-score e (- effort 1))) expr)))))))

(define (op:small-procedure? val)
  (<= (op:compute-score val op:*cp0-effort-limit*) op:*cp0-score-limit*))

(define (op:should-inline? var val body)
  (let ((count (op:count-occurrences var body)))
    (or (<= count 1)
        (and (op:small-procedure? val)
             (let ((depth (hash-table-get op:*inlining-depth* var 0)))
               (< depth 2)))))) ;; Unroll recursive calls once

(define (op:count-occurrences var expr)
  (cond
    ((eq? expr var) 1)
    ((not (pair? expr)) 0)
    ((eq? (car expr) 'quote) 0)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         0
         (apply + (map (lambda (e) (op:count-occurrences var e)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (+ (apply + (map (lambda (b) (op:count-occurrences var (cadr b))) (cadr expr)))
          (if (memq var vars) 0 (apply + (map (lambda (e) (op:count-occurrences var e)) (cddr expr)))))))
    (else (apply + (map (lambda (e) (op:count-occurrences var e)) expr)))))

;; --- Analysis Utilities ---

(define (op:analyze-used-vars expr)
  (cond
    ((symbol? expr) (list expr))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr)) (body (cddr expr)))
       (op:set-minus (op:analyze-used-vars `(begin ,@body)) (if (list? params) params (list params)))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr)) (body (cddr expr)))
       (op:set-union (apply op:set-union (map (lambda (b) (op:analyze-used-vars (cadr b))) bindings))
                  (op:set-minus (op:analyze-used-vars `(begin ,@body)) (map car bindings)))))
    (else (apply op:set-union (map op:analyze-used-vars expr)))))

(define (op:analyze-free-vars expr bound-vars)
  (cond
    ((symbol? expr) (if (memq expr bound-vars) '() (list expr)))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr)) (body (cddr expr)))
       (let ((params-list (if (list? params) params (list params))))
         (op:analyze-free-vars `(begin ,@body) (op:set-union params-list bound-vars)))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr)) (body (cddr expr)))
       (let ((vars (map car bindings)) (vals (map cadr bindings)))
         (op:set-union (apply op:set-union (map (lambda (v) (op:analyze-free-vars v bound-vars)) vals))
                    (op:analyze-free-vars `(begin ,@body) (op:set-union vars bound-vars))))))
    ((eq? (car expr) 'if)
     (op:set-union (op:analyze-free-vars (cadr expr) bound-vars)
                (op:analyze-free-vars (caddr expr) bound-vars)
                (if (null? (cdddr expr)) '() (op:analyze-free-vars (cadddr expr) bound-vars))))
    ((eq? (car expr) 'begin)
     (apply op:set-union (map (lambda (e) (op:analyze-free-vars e bound-vars)) (cdr expr))))
    ((eq? (car expr) 'set!)
     (op:set-union (if (memq (cadr expr) bound-vars) '() (list (cadr expr)))
                (op:analyze-free-vars (caddr expr) bound-vars)))
    ((eq? (car expr) 'define)
     (op:analyze-free-vars (caddr expr) bound-vars))
    (else (apply op:set-union (map (lambda (e) (op:analyze-free-vars e bound-vars)) expr)))))

(define (op:analyze-mutated-vars expr)
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'set!) (cons (cadr expr) (op:analyze-mutated-vars (caddr expr))))
    ((eq? (car expr) 'lambda)
     (let ((params (if (list? (cadr expr)) (cadr expr) (list (cadr expr)))))
       (op:set-minus (apply op:set-union (map op:analyze-mutated-vars (cddr expr))) params)))
    (else (apply op:set-union (map op:analyze-mutated-vars expr)))))

(define (op:mutated? var expr)
  (cond
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'set!) (or (eq? (cadr expr) var) (op:mutated? var (caddr expr))))
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         #f
         (op:any (lambda (e) (op:mutated? var e)) (cddr expr))))
    (else (op:any (lambda (e) (op:mutated? var e)) expr))))

(define (op:has-effects? expr)
  (cond
    ((symbol? expr) #f)
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'lambda) #f)
    ((eq? (car expr) 'if) (or (op:has-effects? (cadr expr)) (op:has-effects? (caddr expr)) (and (not (null? (cdddr expr))) (op:has-effects? (cadddr expr)))))
    ((eq? (car expr) 'begin) (op:any op:has-effects? (cdr expr)))
    ((eq? (car expr) 'set!) #t)
    ((eq? (car expr) 'define) #t)
    ((eq? (car expr) 'let) (or (op:any (lambda (b) (op:has-effects? (cadr b))) (cadr expr)) (op:any op:has-effects? (cddr expr))))
    (else #t))) ;; Assume applications have effects for now

;; --- General Utilities ---

(define (op:set-union . sets)
  (op:fold (lambda (s acc)
          (op:fold (lambda (x a) (if (memq x a) a (cons x a))) acc s))
        '() sets))

(define (op:set-minus s1 s2)
  (filter (lambda (x) (not (memq x s2))) s1))

(define (op:any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (op:any pred (cdr lst)))))

(define (op:fold proc seed lst)
  (if (null? lst) seed
      (op:fold proc (proc (car lst) seed) (cdr lst))))
