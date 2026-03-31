(define *pass-count* 0)
(define *fail-count* 0)

(define (test-equal name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; Minimal stress test - just the ??!apply substitution with 2 vars
(define-syntax ??!apply
  (syntax-rules (??!lambda)
    ((_ (??!lambda (bound-var . other-bound-vars) body) oval . other-ovals)
     (letrec-syntax ((subs
                      (syntax-rules (??! bound-var ??!lambda)
                        ((_ val k (??! bound-var)) (appl k val))
                        ((_ val k (??!lambda bvars int-body))
                         (subs-in-lambda val bvars (k bvars) int-body))
                        ((_ val k (x)) (subs val (recon-pair val k ()) x))
                        ((_ val k (x . y)) (subs val (subsed-cdr val k x) y))
                        ((_ val k x) (appl k x))))
                     (subsed-cdr
                       (syntax-rules ()
                         ((_ val k x new-y)
                          (subs val (recon-pair val k new-y) x))))
                     (recon-pair
                       (syntax-rules ()
                         ((_ val k new-y new-x) (appl k (new-x . new-y)))))
                     (subs-in-lambda
                       (syntax-rules (bound-var)
                         ((_ val () kp int-body)
                          (subs val (recon-l kp ()) int-body))
                         ((_ val (bound-var . obvars) (k bvars) int-body)
                          (appl k (??!lambda bvars int-body)))
                         ((_ val (obvar . obvars) kp int-body)
                          (subs-in-lambda val obvars kp int-body))))
                     (recon-l
                       (syntax-rules ()
                         ((_ (k bvars) () result)
                          (appl k (??!lambda bvars result)))))
                     (appl
                      (syntax-rules ()
                        ((_ (a b c d) result) (a b c d result))
                        ((_ (a b c) result) (a b c result))))
                     (finish
                       (syntax-rules ()
                         ((_ () () exp) exp)
                         ((_ rem-bvars rem-ovals exps)
                          (??!apply (??!lambda rem-bvars exps) . rem-ovals)))))
       (subs oval (finish other-bound-vars other-ovals) body)))))

(define-syntax ?cons (syntax-rules () ((_ x y k) (??!apply k (x . y)))))
(define-syntax ?car (syntax-rules () ((_ (x . y) k) (??!apply k x))))
(define-syntax ?cdr (syntax-rules () ((_ (x . y) k) (??!apply k y))))
(define-syntax ?null? (syntax-rules () ((_ () k) (??!apply k #t)) ((_ x k) (??!apply k #f))))
(define-syntax ?ifnull? (syntax-rules () ((_ () kt kf) (??!apply kt #t)) ((_ x kt kf) (??!apply kf #f))))

;; Simple test: build and test a pair
(define ans #f)
(?cons hello world (??!lambda (x) (set! ans (quote (??! x)))))

(test-equal "check output pair" ans '(hello . world))

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))
