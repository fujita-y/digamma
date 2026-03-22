(load "../core.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

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
