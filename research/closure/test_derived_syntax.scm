;; test_derived_syntax.scm
;; Test suite for derived syntax transformations.

(load "derived_syntax.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (expand-derived-syntax expr)))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write result) (newline)))))

;; =============================================================================
;; Section 1: Let* Transformation
;; =============================================================================
(display "\n>>> Section 1: Let* Transformation\n")

(test "let* Simple"
      '(let* ((x 1)) x)
      '(let ((x 1)) x))

(test "let* Two Bindings"
      '(let* ((x 1) (y 2)) (+ x y))
      '(let ((x 1)) (let ((y 2)) (+ x y))))

(test "let* Sequential Dependency"
      '(let* ((x 10) (y (+ x 5))) y)
      '(let ((x 10)) (let ((y (+ x 5))) y)))

(test "let* Empty Bindings"
      '(let* () 42)
      '(let () 42))

;; =============================================================================
;; Section 2: Letrec and Letrec* Transformation
;; =============================================================================
(display "\n>>> Section 2: Letrec and Letrec* Transformation\n")

(test "letrec Simple"
      '(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))
      '(let ((fact #f)) (set! fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)))

(test "letrec Mutual Recursion"
      '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
         (even? 10))
      '(let ((even? #f) (odd? #f))
         (set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
         (even? 10)))

(test "letrec* Simple"
      '(letrec* ((x 1) (y 2)) (+ x y))
      '(let ((x #f) (y #f)) (set! x 1) (set! y 2) (+ x y)))

;; =============================================================================
;; Section 3: Cond Transformation
;; =============================================================================
(display "\n>>> Section 3: Cond Transformation\n")

(test "cond Simple"
      '(cond ((< x 0) 'negative) ((= x 0) 'zero) (else 'positive))
      '(if (< x 0) 'negative (if (= x 0) 'zero 'positive)))

(test "cond Without Else"
      '(cond ((< x 0) 'negative) ((= x 0) 'zero))
      '(if (< x 0) 'negative (if (= x 0) 'zero #f)))

(test "cond Single Test"
      '(cond (x 'truthy))
      '(if x 'truthy #f))

(test "cond Test Only (returns test value)"
      '(cond (x) (else 'false))
      '(if x x 'false))

;; =============================================================================
;; Section 4: And and Or Transformation
;; =============================================================================
(display "\n>>> Section 4: And and Or Transformation\n")

(test "and Empty" '(and) #t)
(test "and Single" '(and x) 'x)
(test "and Multiple" '(and a b c) '(if a (if b c #f) #f))

(test "or Empty" '(or) #f)
(test "or Single" '(or x) 'x)
(test "or Multiple"
      '(or a b c)
      '(let ((or-temp.1 a)) (if or-temp.1 or-temp.1 (let ((or-temp.2 b)) (if or-temp.2 or-temp.2 c)))))

;; =============================================================================
;; Section 5: Case Transformation
;; =============================================================================
(display "\n>>> Section 5: Case Transformation\n")

(test "case Simple"
      '(case x ((1) 'one) ((2) 'two) (else 'other))
      '(let ((case-key.3 x)) (if (memv case-key.3 '(1)) 'one (if (memv case-key.3 '(2)) 'two 'other))))

(test "case Multiple Values"
      '(case x ((1 2 3) 'small) ((4 5 6) 'medium) (else 'large))
      '(let ((case-key.4 x)) (if (memv case-key.4 '(1 2 3)) 'small (if (memv case-key.4 '(4 5 6)) 'medium 'large))))

;; =============================================================================
;; Section 6: Nested Transformations
;; =============================================================================
(display "\n>>> Section 6: Nested Transformations\n")

(test "Nested let* and cond"
      '(let* ((x 10) (y 20)) (cond ((< x y) (+ x y)) (else (- x y))))
      '(let ((x 10)) (let ((y 20)) (if (< x y) (+ x y) (- x y)))))

(test "Lambda with let*"
      '(lambda (n) (let* ((x (* n 2)) (y (+ x 1))) y))
      '(lambda (n) (let ((x (* n 2))) (let ((y (+ x 1))) y))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
