;; Consolidated tests for test_compiler.scm

(define *pass-count* 0)
(define *fail-count* 0)

;; =============================================================================
;; test_optimizer.scm
;; =============================================================================

;; --- Test Helper Functions ---


(define (test name expr expected)
  (let ((result (optimize expr)))
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

(test "Global constant propagation"
      '(begin (define x 10) (+ x 5))
      '(begin (define x 10) (+ 10 5)))

(test "Unused parameter removal (lambda)"
      '((lambda (x y) x) 1 2)
      '1)

#;(test "Unused parameter removal (with effects)"
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

(test "Lambda Dropping (no drop if used in both)"
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


;; =============================================================================
;; test_licm.scm
;; =============================================================================

;; --- Test Helper Functions ---


(define (test name expr expected)
  (let ((result (optimize expr)))
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

(display "Running LICM Tests...\n")

;; Test 1: Loop with invariant calculation (* x y)
#;(test "LICM Candidate Match"
      '(let ((x 10) (y 20))
         (let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (loop (+ i (* x y)))
                            i)))
           (loop 0)))
      '(let ((licm.1 (* 10 20)))
         (let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (loop (+ i licm.1))
                            i)))
           (loop 0))))

;; Test 2: Nested Invariants
#;(test "Nested Invariants"
      '(let ((a 1) (b 2))
         (let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (begin
                              (display (+ a b))
                              (loop (+ i 1)))
                            i)))
           (loop 0)))
      '(let ((licm.2 (+ 1 2)))
         (let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (begin
                              (display licm.2)
                              (loop (+ i 1)))
                            i)))
           (loop 0))))

;; Test 3: Variant (Not Invariant)
;; i is variant, so (* i 2) is variant.
#;(test "Variant check"
      '(let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (loop (+ i (* i 2)))
                            i)))
           (loop 0))
      '(let ((loop #f))
           (set! loop (lambda (i)
                        (if (< i 10)
                            (loop (+ i (* i 2)))
                            i)))
           (loop 0)))



;; =============================================================================
;; test_lambda-lift.scm
;; =============================================================================


(define (match-lifted? lifted expected)
  (cond ((and (symbol? lifted) (symbol? expected))
         (let ((ls (symbol->string lifted))
               (es (symbol->string expected)))
           ;; If expected has _ suffix, it means it's a lifted function name
           ;; so we just check if the actual name starts with the expected prefix.
           (if (and (> (string-length es) 1)
                    (char=? (string-ref es (- (string-length es) 1)) #\_))
               (and (>= (string-length ls) (string-length es))
                    (string=? (substring ls 0 (string-length es)) es))
               (eq? lifted expected))))
        ((and (pair? lifted) (pair? expected))
         (and (match-lifted? (car lifted) (car expected))
              (match-lifted? (cdr lifted) (cdr expected))))
        (else
         (equal? lifted expected))))

(define (test name expr expected)
  (let* ((lifted (lambda-lift expr))
         (passed? (match-lifted? lifted expected)))
    (if passed?
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write lifted) (newline)))))

(display "\n>>> Lambda Lifting\n")

;; Test 1: Single binding that matches match-rec-pattern should NOT be lifted now
#;(test "Single un-nested recursive let (match-rec-pattern)"
      '(let ((f #f))
         (set! f (lambda (x) (+ x 1)))
         (f 10))
      '(let ((f #f))
         (set! f (lambda (x) (+ x 1)))
         (f 10)))

;; Test 1.5: Single binding that fails match-rec-pattern (self captured in nested lambda) SHOULD lift
(test "Single lift (escaping self in nested lambda)"
      '(let ((f #f))
         (set! f (lambda (x) (lambda () (f x))))
         (f 10))
      '(begin
         (define f_ (lambda (x) (lambda () (f_ x))))
         (f_ 10)))

;; Test 2: Don't lift if lambda has free variables
(test "No lift (free vars)"
      '(let ((y 2))
         (let ((f #f))
           (set! f (lambda (x) (+ x y)))
           (f 10)))
      '(let ((y 2))
         (let ((f #f))
           (set! f (lambda (x) (+ x y)))
           (f 10))))

;; Test 3: Multiple definitions
(test "Multiple lift"
      '(let ((f #f) (g #f))
         (set! f (lambda (x) (g x)))
         (set! g (lambda (y) (f y)))
         (f 1))
      '(begin
         (define f_ (lambda (x) (g_ x)))
         (define g_ (lambda (y) (f_ y)))
         (f_ 1)))

;; Test 4: Partially liftable (one has no free vars, one has free vars)
(test "Partial lift"
      '(let ((z 1))
         (let ((f #f) (g #f))
           (set! f (lambda (x) (g x)))
           (set! g (lambda (y) (+ y z)))
           (f 1)))
      '(let ((z 1))
         (let ((f #f) (g #f))
           (set! f (lambda (x) (g x)))
           (set! g (lambda (y) (+ y z)))
           (f 1))))

;; The user map example
#;(test "Map example (from issue)"
      '(define map
         (lambda (proc.1 lst1.2 . lst2.3)
           (let ((map-1.4 #f) (map-n.5 #f))
             (set! map-1.4
                   (lambda (proc.6 lst.7)
                     (if (null? lst.7) '() (cons (proc.6 (car lst.7)) (map-1.4 proc.6 (cdr lst.7))))))
             (set! map-n.5
                   (lambda (proc.8 lst.9)
                     (if (null? lst.9)
                         '()
                         (cons (apply proc.8 (car lst.9)) (map-n.5 proc.8 (cdr lst.9))))))
             (if (null? lst2.3)
                 (if (list? lst1.2)
                     (map-1.4 proc.1 lst1.2)
                     (error 'map "expected proper list" (cons* proc.1 lst1.2 lst2.3)))
                 (let ((tmp.10.11 (apply list-transpose+ lst1.2 lst2.3)))
                   (if tmp.10.11
                       ((lambda (lst.12) (map-n.5 proc.1 lst.12)) tmp.10.11)
                       (error 'map "expected same length proper lists" (cons* proc.1 lst1.2 lst2.3))))))))
      '(begin
         (define map-1.4_
           (lambda (proc.6 lst.7)
             (if (null? lst.7) '() (cons (proc.6 (car lst.7)) (map-1.4_ proc.6 (cdr lst.7))))))
         (define map-n.5_
           (lambda (proc.8 lst.9)
             (if (null? lst.9)
                 '()
                 (cons (apply proc.8 (car lst.9)) (map-n.5_ proc.8 (cdr lst.9))))))
         (define map
           (lambda (proc.1 lst1.2 . lst2.3)
             (if (null? lst2.3)
                 (if (list? lst1.2)
                     (map-1.4_ proc.1 lst1.2)
                     (error 'map "expected proper list" (cons* proc.1 lst1.2 lst2.3)))
                 (let ((tmp.10.11 (apply list-transpose+ lst1.2 lst2.3)))
                   (if tmp.10.11
                       ((lambda (lst.12) (map-n.5_ proc.1 lst.12)) tmp.10.11)
                       (error 'map "expected same length proper lists" (cons* proc.1 lst1.2 lst2.3)))))))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)


(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin 
      (display "ALL TESTS PASSED.\n") 
      (exit 0))
    (begin
      (display "FAILED ")
      (display *fail-count*) 
      (display " TESTS.\n") 
      (exit 1)))
(newline)
