(define (alpha-conversion expr)
  (let ((counter 0))
    ;; Generate a fresh unique name based on the original symbol
    (define (fresh-name sym)
      (set! counter (+ counter 1))
      (string->symbol (string-append (symbol->string sym) "." (number->string counter))))
    
    ;; Look up a variable in the environment, return original if not found (free var)
    (define (lookup name env)
      (let ((pair (assoc name env)))
        (if pair (cdr pair) name)))

    ;; Rename a pattern of parameters (symbol, list, or improper list)
    ;; Returns (cons new-params new-env)
    (define (rename-params params env)
      (cond
       ((null? params) (cons '() env))
       ((symbol? params) 
        (let ((new-p (fresh-name params)))
          (cons new-p (cons (cons params new-p) env))))
       ((pair? params)
        (let* ((sym (car params))
               (new-sym (fresh-name sym))
               (env-with-car (cons (cons sym new-sym) env))
               (rest-result (rename-params (cdr params) env-with-car))
               (new-cdr (car rest-result))
               (final-env (cdr rest-result)))
          (cons (cons new-sym new-cdr) final-env)))
       (else (error "Invalid parameter structure"))))

    ;; Helper for let-bindings: renames a list of variables in parallel then extends env
    ;; Returns (cons new-vars new-env)
    (define (rename-let-vars vars env)
      (let loop ((vs vars) (acc-vars '()) (acc-env env))
        (if (null? vs)
            (cons (reverse acc-vars) acc-env)
            (let ((new-v (fresh-name (car vs))))
              (loop (cdr vs) (cons new-v acc-vars) (cons (cons (car vs) new-v) acc-env))))))

    (define (transform expr env)
      (cond
       ((symbol? expr) (lookup expr env))
       ((not (pair? expr)) expr)
       ((eq? (car expr) 'quote) expr)
       
       ((eq? (car expr) 'if)
        `(if ,(transform (cadr expr) env)
             ,(transform (caddr expr) env)
             ,(if (null? (cdddr expr)) '() (transform (cadddr expr) env))))
       
       ((eq? (car expr) 'begin)
        `(begin ,@(map (lambda (e) (transform e env)) (cdr expr))))
       
       ((eq? (car expr) 'set!)
        `(set! ,(lookup (cadr expr) env) ,(transform (caddr expr) env)))
       
       ((eq? (car expr) 'lambda)
        (let ((params (cadr expr))
              (body (cddr expr)))
          (let* ((res (rename-params params env))
                 (new-params (car res))
                 (new-env (cdr res)))
            `(lambda ,new-params
               ,@(map (lambda (e) (transform e new-env)) body)))))
       
       ((eq? (car expr) 'let)
        (let ((bindings (cadr expr))
              (body (cddr expr)))
          (let* ((vars (map car bindings))
                 (vals (map cadr bindings))
                 ;; Transform values in current environment (before renaming)
                 (trans-vals (map (lambda (e) (transform e env)) vals))
                 ;; Rename vars
                 (res (rename-let-vars vars env))
                 (new-vars (car res))
                 (new-env (cdr res)))
            `(let ,(map list new-vars trans-vals)
               ,@(map (lambda (e) (transform e new-env)) body)))))
       
       ((eq? (car expr) 'define)
        (if (pair? (cadr expr))
            ;; (define (f x) ...)
            (let* ((head (cadr expr))
                   (name (car head))
                   (args (cdr head))
                   (body (cddr expr)))
              ;; Do not rename 'name'
              (let* ((res-args (rename-params args env))
                     (new-args (car res-args))
                     (body-env (cdr res-args)))
                 `(define (,name ,@new-args)
                    ,@(map (lambda (e) (transform e body-env)) body))))
            ;; (define x val)
            (let ((var (cadr expr))
                  (val (caddr expr)))
              ;; Do not rename 'var'
              `(define ,var ,(transform val env)))))
       
       ;; Application or specialized forms
       (else
        (map (lambda (e) (transform e env)) expr))))

    (transform expr '())))
