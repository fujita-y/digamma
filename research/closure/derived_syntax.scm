;; derived_syntax.scm
;; Transform derived syntax forms to basic syntax

(load "quasiquote.scm")

;; Simple gen-temp implementation
(define gen-temp-counter 0)
(define (gen-temp prefix)
  (set! gen-temp-counter (+ gen-temp-counter 1))
  (string->symbol (string-append prefix "." (number->string gen-temp-counter))))

;; Transform let* to nested let expressions
;; (let* ((x 1) (y 2)) body) => (let ((x 1)) (let ((y 2)) body))
(define (transform-let* expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (if (null? bindings)
        `(let () ,@body)
        (let loop ((bindings bindings))
          (if (null? bindings)
              `(begin ,@body)
              (if (null? (cdr bindings))
                  `(let (,(car bindings)) ,@body)
                  `(let (,(car bindings))
                     ,(loop (cdr bindings)))))))))

;; Transform letrec to let + set!
;; (letrec ((x val)) body) => (let ((x #f)) (set! x val) body)
;; This handles mutual recursion by first binding all vars to undefined,
;; then setting their values
(define (transform-letrec expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (let ((vars (map car bindings))
          (vals (map cadr bindings)))
      `(let ,(map (lambda (v) (list v #f)) vars)
         ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
         ,@body))))

;; Transform letrec* to let + sequential set!
;; Similar to letrec but sets are done sequentially
(define (transform-letrec* expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (let ((vars (map car bindings))
          (vals (map cadr bindings)))
      `(let ,(map (lambda (v) (list v #f)) vars)
         ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
         ,@body))))

;; Transform cond to nested if expressions
;; (cond (test1 expr1) (test2 expr2) (else expr3))
;; => (if test1 expr1 (if test2 expr2 expr3))
(define (transform-cond expr)
  (let ((clauses (cdr expr)))
    (if (null? clauses)
        #f  ;; Empty cond is undefined
        (let loop ((clauses clauses))
          (if (null? clauses)
              #f
              (let ((clause (car clauses)))
                (let ((test (car clause))
                      (exprs (cdr clause)))
                  (if (eq? test 'else)
                      (if (null? exprs)
                          #t
                          (if (null? (cdr exprs))
                              (car exprs)
                              `(begin ,@exprs)))
                      (let ((then-expr (if (null? exprs)
                                          test  ;; (cond (test) ...) returns test if true
                                          (if (null? (cdr exprs))
                                              (car exprs)
                                              `(begin ,@exprs)))))
                        (if (null? (cdr clauses))
                            `(if ,test ,then-expr #f)
                            `(if ,test ,then-expr ,(loop (cdr clauses)))))))))))))

;; Transform and to nested if expressions
;; (and a b c) => (if a (if b c #f) #f)
(define (transform-and expr)
  (let ((args (cdr expr)))
    (cond
     ((null? args) #t)
     ((null? (cdr args)) (car args))
     (else
      `(if ,(car args)
           ,(transform-and `(and ,@(cdr args)))
           #f)))))

;; Transform or to nested if expressions
;; (or a b c) => (let ((temp a)) (if temp temp (or b c)))
(define (transform-or expr)
  (let ((args (cdr expr)))
    (cond
     ((null? args) #f)
     ((null? (cdr args)) (car args))
     (else
      (let ((temp (gen-temp "or-temp")))
        `(let ((,temp ,(car args)))
           (if ,temp
               ,temp
               ,(transform-or `(or ,@(cdr args))))))))))

;; Transform case to cond
;; (case key ((val1) expr1) ((val2 val3) expr2) (else expr3))
;; => (let ((temp key)) (cond ((memv temp '(val1)) expr1) ...))
(define (transform-case expr)
  (let ((key (cadr expr))
        (clauses (cddr expr)))
    (let ((temp (gen-temp "case-key")))
      `(let ((,temp ,key))
         ,(let loop ((clauses clauses))
            (if (null? clauses)
                #f
                (let ((clause (car clauses)))
                  (let ((datum (car clause))
                        (exprs (cdr clause)))
                    (if (eq? datum 'else)
                        (if (null? exprs)
                            #t
                            (if (null? (cdr exprs))
                                (car exprs)
                                `(begin ,@exprs)))
                        (let ((test `(memv ,temp ',(if (list? datum) datum (list datum))))
                              (then-expr (if (null? exprs)
                                           #t
                                           (if (null? (cdr exprs))
                                               (car exprs)
                                               `(begin ,@exprs)))))
                          (if (null? (cdr clauses))
                              `(if ,test ,then-expr #f)
                              `(if ,test ,then-expr ,(loop (cdr clauses))))))))))))))

;; Main transformation function
(define (desugar expr)
  (cond
   ;; Atoms and quotes
   ((not (pair? expr)) expr)
   ((eq? (car expr) 'quote) expr)
   
   ;; Transform derived forms
   ((eq? (car expr) 'let*)
    (desugar (transform-let* expr)))
   
   ((eq? (car expr) 'letrec)
    (desugar (transform-letrec expr)))
   
   ((eq? (car expr) 'letrec*)
    (desugar (transform-letrec* expr)))
   
   ((eq? (car expr) 'cond)
    (desugar (transform-cond expr)))
   
   ((eq? (car expr) 'and)
    (desugar (transform-and expr)))
   
   ((eq? (car expr) 'or)
    (desugar (transform-or expr)))
   
   ((eq? (car expr) 'case)
    (desugar (transform-case expr)))
   
   ;; Expand quasiquote
   ((eq? (car expr) 'quasiquote)
    (desugar (mc:expand-quasiquote (cadr expr))))
   
   ;; Recurse into basic forms
   ((eq? (car expr) 'lambda)
    `(lambda ,(cadr expr)
       ,@(map desugar (cddr expr))))
   
   ((eq? (car expr) 'let)
    `(let ,(map (lambda (b) 
                  (list (car b) (desugar (cadr b))))
                (cadr expr))
       ,@(map desugar (cddr expr))))
   
   ((eq? (car expr) 'if)
    `(if ,(desugar (cadr expr))
         ,(desugar (caddr expr))
         ,@(if (null? (cdddr expr)) 
               '() 
               (list (desugar (cadddr expr))))))
   
   ((eq? (car expr) 'begin)
    `(begin ,@(map desugar (cdr expr))))
   
   ((eq? (car expr) 'set!)
    `(set! ,(cadr expr) ,(desugar (caddr expr))))
   
   ((eq? (car expr) 'define)
    (if (pair? (cadr expr))
        ;; (define (f x) body)
        `(define ,(cadr expr)
           ,@(map desugar (cddr expr)))
        ;; (define x val)
        `(define ,(cadr expr)
           ,(desugar (caddr expr)))))
   
   ;; Application
   (else
    (map desugar expr))))

;; Main entry point
(define (expand-derived-syntax expr)
  (desugar expr))
