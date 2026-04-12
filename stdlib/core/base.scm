(define-module (core base)
  (export do when unless)

  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))

  (define-syntax do
    (syntax-rules ()
      ((do ((var init step) ...)
           (test expr ...)
           body ...)
      (letrec ((loop (lambda (var ...)
                        (if test
                            (begin expr ...)
                            (begin body ... (loop step ...))))))
        (loop init ...)))))
) 