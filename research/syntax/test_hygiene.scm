;; test_hygiene.scm
(load "macroexpand.scm")

(define (assert-equal expected actual msg)
  (if (equal? expected actual)
      (begin (display "PASS: ") (display msg) (newline))
      (begin (display "FAIL: ") (display msg) (newline)
             (display "  Expected: ") (display expected) (newline)
             (display "  Actual:   ") (display actual) (newline))))

;; Hygiene Test 1: Variable Capture
;; The macro introduces 'temp'. We use 'temp' as an argument.
;; If unhygienic, the macro's 'temp' will shadow the user's 'temp'.

(expand '(define-syntax swap
           (syntax-rules ()
             ((_ a b)
              (let ((temp a))
                (set! a b)
                (set! b temp))))))

;; We can't easily execute the result efficiently in this mock expander without an evaluator, 
;; but we can check the expanded code structure if we had renaming.
;; Since we don't have an evaluator in verify step usually, we rely on expansion structure.
;; BUT, for research purposes, if we only produce symbols, we can simulate the issue by
;; checking if the bound variable name is exactly 'temp'.

(define result (expand '(let ((temp 1) (other 2))
                          (swap temp other))))

;; Unhygienic result often looks like:
;; (let ((temp 1) (other 2))
;;   (let ((temp temp))  <-- Captured!
;;     (set! temp other)
;;     (set! other temp)))

(display "Expansion result: ")
(display result)
(newline)

;; In a hygienic expansion, the inner 'temp' should be renamed, e.g., 'temp.1'.
;; So we check that the let-bound variable is NOT 'temp'.

(define inner-let (caddr result)) ;; (let (...) (swap ...)) -> (let (...) (let ...))
(define inner-binding (car (cadr inner-let))) ;; (temp temp)
(define inner-var (car inner-binding))

(if (eq? inner-var 'temp)
    (begin
      (display "FAIL: Hygiene test - 'temp' was captured.\n")
      (assert-equal "not temp" inner-var "Inner variable should be renamed"))
    (begin
      (display "PASS: Hygiene test - 'temp' appeared to be renamed (or at least different).\n")))
