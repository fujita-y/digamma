;; constant-folding.scm
;; Constant Folding Optimization for Scheme

;; Check if an expression is a constant value
(define (constant? expr)
  (or (number? expr)
      (boolean? expr)
      (char? expr)
      (string? expr)
      (and (pair? expr) (eq? (car expr) 'quote))))

;; Get the actual value of a constant expression
(define (constant-value expr)
  (if (and (pair? expr) (eq? (car expr) 'quote))
      (cadr expr)
      expr))

;; Safe division that returns #f if division by zero
(define (safe-div a b)
  (if (= b 0) #f (/ a b)))

;; Fold arithmetic operations
(define (fold-arithmetic op args)
  (cond
   ((eq? op '+)
    (if (null? args) 0 (apply + args)))
   ((eq? op '-)
    (if (null? args) 
        0
        (if (null? (cdr args))
            (- (car args))
            (apply - args))))
   ((eq? op '*)
    (if (null? args) 1 (apply * args)))
   ((eq? op '/)
    (if (or (null? args) (null? (cdr args)))
        #f  ;; Can't fold single arg division without knowing context
        (let ((result (safe-div (car args) (cadr args))))
          (if result
              (if (null? (cddr args))
                  result
                  (let loop ((acc result) (rest (cddr args)))
                    (if (null? rest)
                        acc
                        (let ((next (safe-div acc (car rest))))
                          (if next
                              (loop next (cdr rest))
                              #f)))))
              #f))))
   (else #f)))

;; Fold comparison operations
(define (fold-comparison op args)
  (cond
   ((eq? op '=) (apply = args))
   ((eq? op '<) (apply < args))
   ((eq? op '>) (apply > args))
   ((eq? op '<=) (apply <= args))
   ((eq? op '>=) (apply >= args))
   (else #f)))

;; Fold boolean operations
(define (fold-boolean op args)
  (cond
   ((eq? op 'not)
    (if (and (= (length args) 1)
             (boolean? (car args)))
        (not (car args))
        #f))
   ((eq? op 'and)
    (cond
     ((null? args) #t)
     ((memq #f args) #f)
     ((andmap boolean? args) (andmap (lambda (x) x) args))
     (else #f)))
   ((eq? op 'or)
    (cond
     ((null? args) #f)
     ((memq #t args) #t)
     ((andmap boolean? args) (ormap (lambda (x) x) args))
     (else #f)))
   (else #f)))

;; Helper: andmap
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

;; Helper: ormap
(define (ormap pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (ormap pred (cdr lst)))))

;; Constant folding optimization
(define (constant-folding expr)
  (cond
   ;; Constants remain unchanged
   ((constant? expr) expr)
   
   ;; Variables remain unchanged
   ((symbol? expr) expr)
   
   ;; Non-pairs remain unchanged
   ((not (pair? expr)) expr)
   
   ;; Quote remains unchanged
   ((eq? (car expr) 'quote) expr)
   
   ;; Lambda: recurse into body
   ((eq? (car expr) 'lambda)
    `(lambda ,(cadr expr)
       ,@(map constant-folding (cddr expr))))
   
   ;; Define: recurse into value/body
   ((eq? (car expr) 'define)
    (if (pair? (cadr expr))
        ;; (define (f x) body)
        `(define ,(cadr expr)
           ,@(map constant-folding (cddr expr)))
        ;; (define x val)
        `(define ,(cadr expr)
           ,(constant-folding (caddr expr)))))
   
   ;; If: evaluate condition if constant
   ((eq? (car expr) 'if)
    (let ((cond-folded (constant-folding (cadr expr)))
          (then-folded (constant-folding (caddr expr)))
          (else-folded (if (null? (cdddr expr)) 
                          '()
                          (constant-folding (cadddr expr)))))
      (if (constant? cond-folded)
          (let ((cond-val (constant-value cond-folded)))
            (if cond-val
                then-folded
                (if (null? (cdddr expr))
                    #f  ;; undefined in Scheme, but we use #f
                    else-folded)))
          `(if ,cond-folded ,then-folded 
               ,@(if (null? (cdddr expr)) '() (list else-folded))))))
   
   ;; Let: recurse into bindings and body
   ((eq? (car expr) 'let)
    (let ((bindings (cadr expr))
          (body (cddr expr)))
      `(let ,(map (lambda (b) 
                    (list (car b) (constant-folding (cadr b))))
                  bindings)
         ,@(map constant-folding body))))
   
   ;; Begin: recurse and flatten
   ((eq? (car expr) 'begin)
    (let ((folded (map constant-folding (cdr expr))))
      ;; Remove nested begins
      (let ((flattened (apply append 
                              (map (lambda (e)
                                     (if (and (pair? e) (eq? (car e) 'begin))
                                         (cdr e)
                                         (list e)))
                                   folded))))
        (if (= (length flattened) 1)
            (car flattened)
            `(begin ,@flattened)))))
   
   ;; Set!: recurse into value
   ((eq? (car expr) 'set!)
    `(set! ,(cadr expr) ,(constant-folding (caddr expr))))
   
   ;; Application: try to fold if operator and all args are constants
   (else
    (let ((folded-expr (map constant-folding expr)))
      (let ((op (car folded-expr))
            (args (cdr folded-expr)))
        ;; Check if all arguments are constants
        (if (andmap constant? args)
            (let ((const-args (map constant-value args)))
              ;; Try to fold based on operator
              (cond
               ;; Arithmetic operations
               ((memq op '(+ - * /))
                (let ((result (fold-arithmetic op const-args)))
                  (if result result folded-expr)))
               
               ;; Comparison operations
               ((memq op '(= < > <= >=))
                (if (andmap number? const-args)
                    (fold-comparison op const-args)
                    folded-expr))
               
               ;; Boolean operations
               ((memq op '(not and or))
                (let ((result (fold-boolean op const-args)))
                  (if (boolean? result) result folded-expr)))
               
               ;; Other operations - don't fold
               (else folded-expr)))
            ;; Not all args are constants, return folded subexpressions
            folded-expr))))))

;; Main entry point
(define (optimize-constants expr)
  (constant-folding expr))
