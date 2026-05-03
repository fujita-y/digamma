;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-module (core base)
  (export do when unless)

  (define-syntax when
    (syntax-rules () 
      ((when test result1 result2 ...)
       (if test (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules () 
      ((unless test result1 result2 ...) 
       (if test #f (begin result1 result2 ...)))))

  (define-syntax do-step 
    (syntax-rules () 
      ((do-step var) var) 
      ((do-step var step) step)))

  (define-syntax do
    (syntax-rules ()
      ((do ((var init step ...) ...) (test expr ...) command ...)
       (let loop ((var init) ...)
         (if test (begin #f expr ...) (begin command ... (loop (do-step var step ...) ...)))))))

) ;[end]