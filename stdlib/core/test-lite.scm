(define-module (core test-lite)
  (export test-begin test-end test-comment test-report
          test-eval! test-eq test-eqv test-equal)
  (import (core struct) (core parameterize))

  (define-struct section (name pass-count fail-count skip-count skip-list on-test on-final env lib))

  (define test-result (make-parameter ""))

  (define test-report (lambda () (format #t "~%~a~%~!" (test-result)) (test-result "") (unspecified)))

  (define copy-primitive-environment
    (lambda ()
      (let ((env (make-environment (gensym "testing-env"))))
        (let ((variables (map car (hashtable->alist (environment-variables (primitive-environment)))))
              (macros (map car (hashtable->alist (environment-macros (primitive-environment))))))
          (copy-environment-variables! (primitive-environment) env variables)
          (copy-environment-macros! (primitive-environment) env macros)
          env))))

  (define section-reset
    (lambda (sec)
      (section-name-set! sec "")
      (section-pass-count-set! sec 0)
      (section-fail-count-set! sec 0)
      (section-skip-count-set! sec 0)
      (section-skip-list-set! sec '())
      (section-on-final-set! sec (lambda x #f))
      (section-on-test-set! sec (lambda x #f))
      (section-env-set! sec (copy-primitive-environment))
      (section-lib-set! sec (list-copy (current-module-registry)))
      sec))

  (define section-current (make-parameter #f))

  (define section-pass-count-inc!
    (lambda (sec)
      (section-pass-count-set! sec (+ (section-pass-count sec) 1))))

  (define section-fail-count-inc!
    (lambda (sec)
      (section-fail-count-set! sec (+ (section-fail-count sec) 1))))

  (define test-default-on-final-proc
    (lambda (sec)
      (let ((report (format "section ~s passed: ~a failed: ~a skipped: ~a"
                            (section-name sec)
                            (section-pass-count sec)
                            (section-fail-count sec)
                            (section-skip-count sec))))
        (and (> (section-fail-count sec) 0)
             (set! report (string-append report "  ; ### TEST FAILURE ###\n")))
        (test-result (string-append (test-result) report "\n"))
        (cond ((> (section-fail-count sec) 0)
               (newline)
               (exit #f))))))

  (define test-default-on-test-proc
    (lambda (sec test passed? form expect got)
      (cond (passed?
             (section-pass-count-inc! sec)
             (format #t "\rpassed ~a~!" (section-pass-count sec)))
            (else
             (section-fail-count-inc! sec)
             (format #t "~%; *** ### TEST FAILURE ###")
             (format #t "~%; *** section: ~s" (section-name sec))
             (format #t "~%; *** name   : ~s" test)
             (format #t "~%; *** expect : ~s" expect)
             (format #t "~%; *** got    : ~s" got)
             (format #t "~%~!" got)
             (exit #f)))))

  (define test-expression
    (lambda (name expr expect pred)
      (let ((sec (section-current)))
        ((lambda (got) ((section-on-test sec) sec name (pred expect got) expr expect got))
         (parameterize ((current-module-registry (section-lib (section-current))))
           (eval expr (section-env (section-current))))))))

  (define test-begin
    (lambda (name)
      (let ((sec (section-reset (make-section #f #f #f #f #f #f #f #f #f))))
        (section-name-set! sec name)
        (section-on-test-set! sec test-default-on-test-proc)
        (section-on-final-set! sec test-default-on-final-proc)
        (section-current sec)
        (format #t "~a~%~!" name)
        (unspecified))))

  (define test-end
    (lambda ()
      (let ((sec (section-current)))
        ((section-on-final sec) sec)
        (format #t "~%~!"))))

  (define test-comment
    (lambda (comment)
      (format #t "\rcomment: ~s~%~!" comment)))

  (define-syntax test-eval!
    (syntax-rules ()
      ((_ expr)
       (parameterize ((current-module-registry (section-lib (section-current))))
         (eval 'expr (section-env (section-current)))))))

  (define-syntax test-eq
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value eq?))
      ((_ expr => value)
       (test-expression "" 'expr 'value eq?))
      ((_ name value expr)
       (test-expression name 'expr 'value eq?))))

  (define-syntax test-eqv
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value eqv?))
      ((_ expr => value)
       (test-expression "" 'expr 'value eqv?))
      ((_ name value expr)
       (test-expression name 'expr 'value eqv?))))

  (define-syntax test-equal
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value equal?))
      ((_ expr => value)
       (test-expression "" 'expr 'value equal?))
      ((_ name value expr)
       (test-expression name 'expr 'value equal?))))

  )
