(load "optimizer.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (op:optimize expr)))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  input:    ") (write expr) (newline)
          (display "  expected: ") (write expected) (newline)
          (display "  actual:   ") (write result) (newline)))))

(display "Running Optimizer Tests...\n")

(test "Constant folding (if #t)" 
      '(if #t 1 2) 
      '1)

(test "Constant folding (if #f)" 
      '(if #f 1 2) 
      '2)

(test "Begin pruning" 
      '(begin 'a 'b 3) 
      '3)

(test "Begin flattening"
      '(begin (display 1) (begin (display 2) (display 3)) (display 4))
      '(begin (display 1) (display 2) (display 3) (display 4)))

(test "Dead code elimination (unused let)" 
      '(let ((x 1)) 2) 
      '2)

(test "Dead code elimination (used let)" 
      '(let ((x 1)) x) 
      '1)

(test "Beta-reduction" 
      '((lambda (x) x) 42) 
      '42)

(test "Nested optimizations" 
      '((lambda (x) (if #t x 2)) 42) 
      '42)

(test "Let-floating"
      '(let ((x (let ((y (unknown))) (cons y y)))) x)
      '(let ((y (unknown))) (cons y y)))

(test "Complex DCE and Beta"
      '(let ((f (lambda (x) (+ x 1)))) (f 5))
      '(+ 5 1))

(test "Side effects protection"
      '(let ((x (set! y 1))) 2)
      '(let ((x (set! y 1))) 2))

(test "If-lifting"
      '(if (if a b c) d e)
      '(if a (if b d e) (if c d e)))

(test "Boolean simplification"
      '(if (> x 0) #t #f)
      '(> x 0))

(test "Global constant propagation"
      '(begin (define x 10) (+ x 5))
      '(begin (define x 10) (+ 10 5)))

(test "Unused parameter removal (lambda)"
      '((lambda (x y) x) 1 2)
      '1)

(test "Unused parameter removal (with effects)"
      '((lambda (x y) x) 1 (display "hello"))
      '(begin (display "hello") 1))

(test "Recursive unrolling (one level)"
      '(let ((f (lambda (n) (if (= n 0) 1 (f (- n 1)))))) (f 3))
      '(if (= 3 0) 1 (f (- 3 1))))

;; --- Lambda Optimization Tests ---

(test "Lambda Dropping (onto then branch)"
      '(let ((f (lambda (x) (+ x 1)))) (if c (f 10) 20))
      '(if c (+ 10 1) 20))

(test "Lambda Dropping (onto else branch)"
      '(let ((f (lambda (x) (+ x 1)))) (if c 10 (f 20)))
      '(if c 10 (+ 20 1)))

(test "Lambda Dropping (no op:drop if used in both)"
      '(let ((f (lambda (x) (+ x 1)))) (if c (f 10) (f 20)))
      '(if c (+ 10 1) (+ 20 1)))

(test "Inlining (pure lambda)"
      '(let ((x 1)) (let ((f (lambda (y) (+ y 1)))) (f x)))
      '(+ 1 1))

(test "Inlining (lambda with free vars)"
      '(let ((x 1)) (let ((f (lambda (y) (+ y x)))) (f 10)))
      '(+ 10 1))

(test "Pure primitive substitution (car)"
      '(let ((x '(1 2 3))) (let ((y (car x))) y))
      '(car '(1 2 3)))

(test "Pure primitive substitution (cdr)"
      '(let ((x '(1 2 3))) (let ((y (cdr x))) y))
      '(cdr '(1 2 3)))

(test "Pure primitive substitution (cons)"
      '(let ((x '(1 2 3))) (let ((y (cons 1 x))) y))
      '(cons 1 '(1 2 3)))

(test "Pure primitive substitution (null?)"
      '(let ((x '(1 2 3))) (let ((y (null? x))) y))
      '(null? '(1 2 3)))

(test "Pure primitive substitution (eq?) - single use"
      '(let ((x '(1 2 3))) (let ((y (eq? x x))) y))
      '(eq? '(1 2 3) '(1 2 3)))

(test "Pure primitive substitution (eq?) - multiple uses (no substitution)"
      '(let ((x '(1 2 3))) (let ((y (eq? x x))) (list y y)))
      '(let ((y (eq? '(1 2 3) '(1 2 3)))) (list y y)))

(test "Pure primitive substitution (null?) - mutation (no substitution)"
      '(let ((x '(1 2 3))) (let ((y (null? x))) (set! y #t)))
      '(let ((y (null? '(1 2 3)))) (set! y #t)))

(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
