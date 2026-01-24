;; test_macroexpand.scm
(load "macroexpand.scm")

(define (assert-equal expected actual msg)
  (if (equal? expected actual)
      (begin
        (display "PASS: ")
        (display msg)
        (newline))
      (begin
        (display "FAIL: ")
        (display msg)
        (newline)
        (display "  Expected: ")
        (display expected)
        (newline)
        (display "  Actual:   ")
        (display actual)
        (newline))))

;; Test 1: Simple macro
(define-syntax my-or
  (syntax-rules ()
    ((_ a b)
     (let ((t a))
       (if t t b)))))

(assert-equal ''defined
              (expand '(define-syntax my-or
                         (syntax-rules ()
                           ((_ a b)
                            (let ((t a))
                              (if t t b))))))
              "Register my-or")

(assert-equal '(let ((t.1 #t)) (if t.1 t.1 #f))
              (expand '(my-or #t #f))
              "Expand my-or")


;; Test 2: Recursive macro (my-and)
;; (and) -> #t
;; (and x) -> x
;; (and x y ...) -> (if x (and y ...) #f)

(expand '(define-syntax my-and
           (syntax-rules ()
             ((_) #t)
             ((_ x) x)
             ((_ x y ...)
              (if x (my-and y ...) #f)))))

(assert-equal '#t
              (expand '(my-and))
              "Expand (my-and)")

(assert-equal '1
              (expand '(my-and 1))
              "Expand (my-and 1)")

(assert-equal '(if 1 (if 2 3 #f) #f)
              (expand '(my-and 1 2 3))
              "Expand (my-and 1 2 3)")

;; Test 3: List literal in pattern
(expand '(define-syntax method
           (syntax-rules (=>)
             ((_ (name args ...) => body)
              (define (name args ...) body)))))

(assert-equal '(define (add x y) (+ x y))
              (expand '(method (add x y) => (+ x y)))
              "Expand method with literal =>")

;; Test 4: Nested expansion (let uses lambda)
;; Note: our expand function handles let, lambda etc.
(assert-equal '(lambda (x) x)
              (expand '(lambda (x) x))
              "Lambda identity")
