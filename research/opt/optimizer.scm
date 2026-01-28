
;;; Optimizer for Scheme Core Forms
;;; Core Forms: [lambda, let, define, if, set!, quote, begin] and applications.

;; --- Global State ---

(define global-env (make-hash-table 'eq?))
(define *inlining-depth* (make-hash-table 'eq?))

;; --- Core API ---

(define (optimize expr)
  (hash-table-clear! global-env)
  (hash-table-clear! *inlining-depth*)
  (let loop ((current expr) (prev '()) (iters 0))
    (if (or (equal? current prev) (>= iters 10))
        current
        (loop (optimize-once-inner current) current (+ iters 1)))))

(define (optimize-once expr)
  (optimize-once-inner expr))

(define (boolean-true? x) (or (eq? x #t) (equal? x ''#t)))
(define (boolean-false? x) (or (eq? x #f) (equal? x ''#f) (null? x)))

;; --- Dispatcher ---

(define (optimize-once-inner expr)
  (cond
    ((symbol? expr) 
     (if (hash-table-exists? global-env expr)
         (hash-table-get global-env expr)
         expr))
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'if) (opt-if expr))
    ((eq? (car expr) 'begin) (opt-begin expr))
    ((eq? (car expr) 'lambda) (opt-lambda expr))
    ((eq? (car expr) 'let) (opt-let expr))
    ((eq? (car expr) 'set!)
     `(set! ,(cadr expr) ,(optimize-once-inner (caddr expr))))
    ((eq? (car expr) 'define)
     (let ((var (cadr expr)) (val (optimize-once-inner (caddr expr))))
       (if (and (not (pair? val)) (not (symbol? val))) ;; Simple constant
           (hash-table-put! global-env var val))
       (if (and (pair? val) (eq? (car val) 'quote))
           (hash-table-put! global-env var val))
       `(define ,var ,val)))
    (else (opt-app expr))))

;; --- Specialized Optimizers ---

(define (opt-if expr)
  (let ((test (optimize-once-inner (cadr expr)))
        (then (optimize-once-inner (caddr expr)))
        (else (if (null? (cdddr expr)) ''#f (optimize-once-inner (cadddr expr)))))
    (cond
      ;; If-lifting: (if (if a b c) d e) -> (if a (if b d e) (if c d e))
      ((and (pair? test) (eq? (car test) 'if))
       (let ((a (cadr test)) (b (caddr test)) (c (cadddr test)))
         (optimize-once-inner `(if ,a (if ,b ,then ,else) (if ,c ,then ,else)))))
      ;; Boolean simplification: (if a #t #f) -> a (if a is boolean)
      ((and (boolean-true? then) (boolean-false? else))
       test)
      ((and (pair? test) (eq? (car test) 'quote))
       (if (cadr test) then else))
      ((boolean? test)
       (if test then else))
      (else `(if ,test ,then ,else)))))

(define (opt-begin expr)
  (let* ((exprs (map optimize-once-inner (cdr expr)))
         (flattened (apply append (map (lambda (x) (if (and (pair? x) (eq? (car x) 'begin)) (cdr x) (list x))) exprs)))
         (filtered (filter (lambda (x) (has-effects? x)) (take flattened (- (length flattened) 1)))))
    (let ((last-val (car (reverse flattened))))
      (if (null? filtered)
          last-val
          `(begin ,@filtered ,last-val)))))

(define (opt-lambda expr)
  (let* ((params (cadr expr))
         (body-exprs (map optimize-once-inner (cddr expr)))
         (body `(begin ,@body-exprs))
         (used (analyze-used-vars body))
         (mutated (analyze-mutated-vars body)))
    ;; Unused parameter removal
    (let loop ((ps (if (list? params) params '())) (i 0) (new-params '()))
      (cond
        ((null? ps) `(lambda ,(if (list? params) (reverse new-params) params) ,@body-exprs))
        ((and (not (memq (car ps) used)) (not (memq (car ps) mutated)))
         (loop (cdr ps) (+ i 1) new-params)) ;; Remove it
        (else (loop (cdr ps) (+ i 1) (cons (car ps) new-params)))))))

(define (opt-let expr)
  (let* ((bindings (map (lambda (b) (list (car b) (optimize-once-inner (cadr b)))) (cadr expr)))
         (body-exprs (map optimize-once-inner (cddr expr)))
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
          (optimize-once-inner `(let ,floated-bindings (let ,main-bindings ,@body)))
          
          ;; Binding Optimization Pipeline
          (let* ((used (analyze-used-vars `(begin ,@body)))
                 (mutated-vars (analyze-mutated-vars `(begin ,@body)))
                 (new-bindings '()))
            
            (define (process-binding! b)
              (let ((var (car b)) (val (cadr b)))
                (cond
                  ;; 1. Copy Propagation
                  ((and (not (memq var mutated-vars)) (or (not (pair? val)) (and (pair? val) (eq? (car val) 'quote)) (symbol? val)))
                   (set! body (map (lambda (e) (substitute e var val)) body))
                   (if (has-effects? val) (set! new-bindings (cons b new-bindings))))
                  
                  ;; 2. Inlining
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (should-inline? var val `(begin ,@body)))
                   (set! body (perform-inlining var val body))
                   (if (has-effects? val) (set! new-bindings (cons b new-bindings))))
                  
                  ;; 3. Lambda Dropping
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (and (pair? (car body)) (eq? (car (car body)) 'if)))
                   (let ((res (try-drop-lambda var val (car body))))
                     (if (car res)
                         (set! body (list (cdr res)))
                         (set! new-bindings (cons b new-bindings)))))
                  
                  ;; 4. Lambda Lifting (Handled by Let-floating in next pass)
                  ((and (not (memq var mutated-vars)) (pair? val) (eq? (car val) 'lambda) (null? (analyze-free-vars val '())))
                   (set! new-bindings (cons b new-bindings)))
                  
                  ;; 5. Keep it if used or has effects
                  ((or (memq var used) (memq var mutated-vars) (has-effects? val))
                   (set! new-bindings (cons b new-bindings))))))

            (for-each process-binding! main-bindings)
            (set! new-bindings (reverse new-bindings))
            
            (if (null? new-bindings)
                (if (null? (cdr body)) (car body) `(begin ,@body))
                `(let ,new-bindings ,@body)))))))

(define (try-drop-lambda var val if-expr)
  (let* ((test (cadr if-expr))
         (then (caddr if-expr))
         (else (if (null? (cdddr if-expr)) ''#f (cadddr if-expr)))
         (used-in-test (memq var (analyze-used-vars test)))
         (used-in-then (memq var (analyze-used-vars then)))
         (used-in-else (memq var (analyze-used-vars else))))
    (cond
      ((and (not used-in-test) used-in-then (not used-in-else))
       (cons #t `(if ,test (let ((,var ,val)) ,then) ,else)))
      ((and (not used-in-test) (not used-in-then) used-in-else)
       (cons #t `(if ,test ,then (let ((,var ,val)) ,else))))
      (else (cons #f #f)))))

(define (opt-app expr)
  (let ((proc (optimize-once-inner (car expr)))
        (args (map optimize-once-inner (cdr expr))))
    (cond
      ;; Beta-reduction: ((lambda (x) ...) y) -> (let ((x y)) ...)
      ((and (pair? proc) (eq? (car proc) 'lambda))
       (let ((params (cadr proc))
             (body (cddr proc)))
         (if (= (length params) (length args))
             (optimize-once `(let ,(map list params args) ,@body))
             ;; Handle argument mismatch
             (if (< (length params) (length args))
                 (let ((extra-args (drop args (length params))))
                   (optimize-once `(begin ,@extra-args (let ,(map list params (take args (length params))) ,@body))))
                 (cons proc args)))))
      (else (cons proc args)))))

;; --- Transformation Helpers ---

(define (take lst n)
  (if (or (null? lst) (<= n 0)) '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (null? lst) (<= n 0)) lst
      (drop (cdr lst) (- n 1))))

(define (substitute expr var val)
  (cond
    ((eq? expr var) val)
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         expr
         `(lambda ,(cadr expr) ,@(map (lambda (e) (substitute e var val)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (if (memq var vars)
           `(let ,(map (lambda (b) (list (car b) (substitute (cadr b) var val))) (cadr expr)) ,@(cddr expr))
           `(let ,(map (lambda (b) (list (car b) (substitute (cadr b) var val))) (cadr expr)) ,@(map (lambda (e) (substitute e var val)) (cddr expr))))))
    (else (map (lambda (e) (substitute e var val)) expr))))

(define (substitute-proc expr var val)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         expr
         `(lambda ,(cadr expr) ,@(map (lambda (e) (substitute-proc e var val)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (if (memq var vars)
           `(let ,(map (lambda (b) (list (car b) (substitute-proc (cadr b) var val))) (cadr expr)) ,@(cddr expr))
           `(let ,(map (lambda (b) (list (car b) (substitute-proc (cadr b) var val))) (cadr expr)) ,@(map (lambda (e) (substitute-proc e var val)) (cddr expr))))))
    ((eq? (car expr) var) 
     (cons val (map (lambda (e) (substitute-proc e var val)) (cdr expr))))
    (else (let ((new-expr (map (lambda (e) (substitute-proc e var val)) expr)))
            (if (and (pair? (car new-expr)) (eq? (car (car new-expr)) 'lambda))
                (optimize-once new-expr) ;; Optimization opportunity (beta-reduction)
                new-expr)))))

(define (perform-inlining var val body)
  (let ((depth (hash-table-get *inlining-depth* var 0)))
    (hash-table-put! *inlining-depth* var (+ depth 1))
    (let ((new-body (map (lambda (e) (substitute-proc e var val)) body)))
      new-body)))

;; --- Inlining Heuristics ---

(define *cp0-effort-limit* 100)
(define *cp0-score-limit* 20)

(define (compute-score expr effort)
  (if (<= effort 0) 1000 ;; Too much effort
      (cond
        ((not (pair? expr)) 1)
        ((eq? (car expr) 'quote) 1)
        ((eq? (car expr) 'if)
         (+ 1 (compute-score (cadr expr) (- effort 1))
            (max (compute-score (caddr expr) (- effort 1))
                 (compute-score (cadddr expr) (- effort 1)))))
        ((eq? (car expr) 'lambda)
         (+ 5 (compute-score (cddr expr) (- effort 1))))
        (else (+ 1 (apply + (map (lambda (e) (compute-score e (- effort 1))) expr)))))))

(define (small-procedure? val)
  (<= (compute-score val *cp0-effort-limit*) *cp0-score-limit*))

(define (should-inline? var val body)
  (let ((count (count-occurrences var body)))
    (or (<= count 1)
        (and (small-procedure? val)
             (let ((depth (hash-table-get *inlining-depth* var 0)))
               (< depth 2)))))) ;; Unroll recursive calls once

(define (count-occurrences var expr)
  (cond
    ((eq? expr var) 1)
    ((not (pair? expr)) 0)
    ((eq? (car expr) 'quote) 0)
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         0
         (apply + (map (lambda (e) (count-occurrences var e)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let ((vars (map car (cadr expr))))
       (+ (apply + (map (lambda (b) (count-occurrences var (cadr b))) (cadr expr)))
          (if (memq var vars) 0 (apply + (map (lambda (e) (count-occurrences var e)) (cddr expr)))))))
    (else (apply + (map (lambda (e) (count-occurrences var e)) expr)))))

;; --- Analysis Utilities ---

(define (analyze-used-vars expr)
  (cond
    ((symbol? expr) (list expr))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr)) (body (cddr expr)))
       (set-minus (analyze-used-vars `(begin ,@body)) (if (list? params) params (list params)))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr)) (body (cddr expr)))
       (set-union (apply set-union (map (lambda (b) (analyze-used-vars (cadr b))) bindings))
                  (set-minus (analyze-used-vars `(begin ,@body)) (map car bindings)))))
    (else (apply set-union (map analyze-used-vars expr)))))

(define (analyze-free-vars expr bound-vars)
  (cond
    ((symbol? expr) (if (memq expr bound-vars) '() (list expr)))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda)
     (let ((params (cadr expr)) (body (cddr expr)))
       (let ((params-list (if (list? params) params (list params))))
         (analyze-free-vars `(begin ,@body) (set-union params-list bound-vars)))))
    ((eq? (car expr) 'let)
     (let ((bindings (cadr expr)) (body (cddr expr)))
       (let ((vars (map car bindings)) (vals (map cadr bindings)))
         (set-union (apply set-union (map (lambda (v) (analyze-free-vars v bound-vars)) vals))
                    (analyze-free-vars `(begin ,@body) (set-union vars bound-vars))))))
    ((eq? (car expr) 'if)
     (set-union (analyze-free-vars (cadr expr) bound-vars)
                (analyze-free-vars (caddr expr) bound-vars)
                (if (null? (cdddr expr)) '() (analyze-free-vars (cadddr expr) bound-vars))))
    ((eq? (car expr) 'begin)
     (apply set-union (map (lambda (e) (analyze-free-vars e bound-vars)) (cdr expr))))
    ((eq? (car expr) 'set!)
     (set-union (if (memq (cadr expr) bound-vars) '() (list (cadr expr)))
                (analyze-free-vars (caddr expr) bound-vars)))
    ((eq? (car expr) 'define)
     (analyze-free-vars (caddr expr) bound-vars))
    (else (apply set-union (map (lambda (e) (analyze-free-vars e bound-vars)) expr)))))

(define (analyze-mutated-vars expr)
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'set!) (cons (cadr expr) (analyze-mutated-vars (caddr expr))))
    ((eq? (car expr) 'lambda)
     (let ((params (if (list? (cadr expr)) (cadr expr) (list (cadr expr)))))
       (set-minus (apply set-union (map analyze-mutated-vars (cddr expr))) params)))
    (else (apply set-union (map analyze-mutated-vars expr)))))

(define (mutated? var expr)
  (cond
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'set!) (or (eq? (cadr expr) var) (mutated? var (caddr expr))))
    ((eq? (car expr) 'lambda)
     (if (memq var (if (list? (cadr expr)) (cadr expr) (list (cadr expr))))
         #f
         (any (lambda (e) (mutated? var e)) (cddr expr))))
    (else (any (lambda (e) (mutated? var e)) expr))))

(define (has-effects? expr)
  (cond
    ((symbol? expr) #f)
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'lambda) #f)
    ((eq? (car expr) 'if) (or (has-effects? (cadr expr)) (has-effects? (caddr expr)) (and (not (null? (cdddr expr))) (has-effects? (cadddr expr)))))
    ((eq? (car expr) 'begin) (any has-effects? (cdr expr)))
    ((eq? (car expr) 'set!) #t)
    ((eq? (car expr) 'define) #t)
    ((eq? (car expr) 'let) (or (any (lambda (b) (has-effects? (cadr b))) (cadr expr)) (any has-effects? (cddr expr))))
    (else #t))) ;; Assume applications have effects for now

;; --- General Utilities ---

(define (set-union . sets)
  (fold (lambda (s acc)
          (fold (lambda (x a) (if (memq x a) a (cons x a))) acc s))
        '() sets))

(define (set-minus s1 s2)
  (filter (lambda (x) (not (memq x s2))) s1))

(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))

(define (fold proc seed lst)
  (if (null? lst) seed
      (fold proc (proc (car lst) seed) (cdr lst))))
