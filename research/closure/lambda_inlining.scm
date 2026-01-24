;; optimize_form.scm
;; Lambda Inlining Optimization

;; Helper: andmap (all elements satisfy predicate)
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

;; Beta reduction: substitute value for variable in expression
(define (substitute expr var val)
  (cond
   ((symbol? expr)
    (if (eq? expr var) val expr))
   ((not (pair? expr)) expr)
   ((eq? (car expr) 'quote) expr)
   ((eq? (car expr) 'lambda)
    (let ((params (cadr expr))
          (body (cddr expr)))
      (let ((flat-params (let loop ((p params))
                          (cond ((null? p) '())
                                ((pair? p) (cons (car p) (loop (cdr p))))
                                (else (list p))))))
        (if (memq var flat-params)
            expr ;; shadowed
            `(lambda ,params ,@(map (lambda (e) (substitute e var val)) body))))))
   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr))
          (body (cddr expr)))
      (let ((vars (map car bindings))
            (vals (map cadr bindings)))
        (if (memq var vars)
            ;; shadowed in body
            `(let ,(map (lambda (v val-expr) 
                          (list v (substitute val-expr var val))) 
                        vars vals)
               ,@body)
            `(let ,(map (lambda (v val-expr) 
                          (list v (substitute val-expr var val))) 
                        vars vals)
               ,@(map (lambda (e) (substitute e var val)) body))))))
   ((eq? (car expr) 'if)
    `(if ,(substitute (cadr expr) var val)
         ,(substitute (caddr expr) var val)
         ,(if (null? (cdddr expr)) '() (substitute (cadddr expr) var val))))
   ((eq? (car expr) 'begin)
    `(begin ,@(map (lambda (e) (substitute e var val)) (cdr expr))))
   ((eq? (car expr) 'set!)
    `(set! ,(cadr expr) ,(substitute (caddr expr) var val)))
   (else
    (map (lambda (e) (substitute e var val)) expr))))

;; Count occurrences of a variable in an expression
(define (count-var-refs expr var)
  (cond
   ((symbol? expr) (if (eq? expr var) 1 0))
   ((not (pair? expr)) 0)
   ((eq? (car expr) 'quote) 0)
   ((eq? (car expr) 'lambda)
    (let ((params (cadr expr))
          (body (cddr expr)))
      (let ((flat-params (let loop ((p params))
                          (cond ((null? p) '())
                                ((pair? p) (cons (car p) (loop (cdr p))))
                                (else (list p))))))
        (if (memq var flat-params)
            0 ;; shadowed
            (apply + (map (lambda (e) (count-var-refs e var)) body))))))
   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr))
          (body (cddr expr)))
      (let ((vars (map car bindings))
            (vals (map cadr bindings)))
        (+ (apply + (map (lambda (v) (count-var-refs v var)) vals))
           (if (memq var vars)
               0 ;; shadowed in body
               (apply + (map (lambda (e) (count-var-refs e var)) body)))))))
   (else
    (apply + (map (lambda (e) (count-var-refs e var)) expr)))))

;; Check if expression is simple (safe to duplicate)
(define (simple-expr? expr)
  (or (symbol? expr)
      (number? expr)
      (boolean? expr)
      (and (pair? expr) (eq? (car expr) 'quote))))

;; Lambda inlining optimization
(define (inline-lambdas expr)
  (cond
   ((not (pair? expr)) expr)
   
   ;; ((lambda (x ...) body) arg ...) -> substitute
   ((and (pair? (car expr))
         (eq? (caar expr) 'lambda))
    (let ((lambda-expr (car expr))
          (args (cdr expr)))
      (let ((params (cadr lambda-expr))
            (body (cddr lambda-expr)))
        (if (not (list? params))
            ;; Don't inline variadic or improper params for now
            expr
            (if (= (length params) (length args))
                ;; Inline if:
                ;; 1. All args are simple, OR
                ;; 2. Each param is used at most once in body
                (let ((can-inline? 
                       (or (andmap simple-expr? args)
                           (andmap (lambda (p) (<= (count-var-refs `(begin ,@body) p) 1))
                                   params))))
                  (if can-inline?
                      ;; Perform substitution
                      (let loop ((ps params) (as args) (result `(begin ,@body)))
                        (if (null? ps)
                            (inline-lambdas result)
                            (loop (cdr ps) 
                                  (cdr as) 
                                  (substitute result (car ps) (inline-lambdas (car as))))))
                      ;; Can't inline, recurse into parts
                      (cons (inline-lambdas lambda-expr)
                            (map inline-lambdas args))))
                ;; Arity mismatch, don't inline
                (cons (inline-lambdas lambda-expr)
                      (map inline-lambdas args)))))))
   
   ;; let -> lambda conversion and inlining
   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr))
          (body (cddr expr)))
      ;; Convert let to lambda application
      (let ((vars (map car bindings))
            (vals (map cadr bindings)))
        ;; Optimize the lambda form
        (inline-lambdas
         `((lambda ,vars ,@body) ,@vals)))))
   
   ;; lambda: recurse into body
   ((eq? (car expr) 'lambda)
    `(lambda ,(cadr expr)
       ,@(map inline-lambdas (cddr expr))))
   
   ;; if: recurse into branches
   ((eq? (car expr) 'if)
    `(if ,(inline-lambdas (cadr expr))
         ,(inline-lambdas (caddr expr))
         ,(if (null? (cdddr expr)) '() (inline-lambdas (cadddr expr)))))
   
   ;; begin: recurse into subexpressions
   ((eq? (car expr) 'begin)
    (let ((optimized (map inline-lambdas (cdr expr))))
      ;; Flatten nested begins
      (let ((flattened (apply append 
                              (map (lambda (e) 
                                     (if (and (pair? e) (eq? (car e) 'begin))
                                         (cdr e)
                                         (list e)))
                                   optimized))))
        (if (= (length flattened) 1)
            (car flattened)
            `(begin ,@flattened)))))
   
   ;; define: recurse into value
   ((eq? (car expr) 'define)
    (if (pair? (cadr expr))
        ;; (define (f x) body)
        `(define ,(cadr expr)
           ,@(map inline-lambdas (cddr expr)))
        ;; (define x val)
        `(define ,(cadr expr)
           ,(inline-lambdas (caddr expr)))))
   
   ;; set!: recurse into value
   ((eq? (car expr) 'set!)
    `(set! ,(cadr expr)
       ,(inline-lambdas (caddr expr))))
   
   ;; default: recurse into all subexpressions
   (else
    (map inline-lambdas expr))))

;; Main optimization entry point
(define (lambda-inlining expr)
  (inline-lambdas expr))
