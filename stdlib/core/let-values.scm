(define-module (core let-values)
  (export let-values let*-values)
  
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
         (let*-values (rest ...) body1 body2 ...))))))

