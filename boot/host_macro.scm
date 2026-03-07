(define-syntax parameterize-aux
  (syntax-rules ()
    ((_ () ((save new param value) ...) body ...)
     (let ((save #f) ... (new value) ...)
        (dynamic-wind
        (lambda () (set! save (param)) ... (param new) ...)
        (lambda () body ...)
        (lambda () (param save) ...))))
    ((_ ((e1 e2) . more) (stash ...) body ...)
     (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((e1 e2) ...) body ...)
     (parameterize-aux ((e1 e2) ...) () body ...))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let-values (((v ...) expr) rest ...) body1 body2 ...)
     (call-with-values
       (lambda () expr)
       (lambda (v ...)
         (let-values (rest ...) body1 body2 ...))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let*-values (binding rest ...) body1 body2 ...)
     (let-values (binding)
       (let*-values (rest ...) body1 body2 ...)))))
