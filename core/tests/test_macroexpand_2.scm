;; test_macroexpand_2.scm
;; Test suite for core macro forms like cond, and, or, case.


(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (macroexpand expr 'strip)))
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
;; Section 1: Testing cond
;; =============================================================================
(display "\n>>> Section 1: Testing cond\n")
(test "cond basic" '(cond (#t 1) (else 2)) '(if #t 1 2))
(test "cond multiple" '(cond (#f 1) (#t 2) (else 3)) '(if #f 1 (if #t 2 3)))
(test "cond arrow" '(cond (1 => (lambda (x) x)) (else 2)) '(let ((tmp 1)) (if tmp ((lambda (x) x) tmp) 2)))

;; =============================================================================
;; Section 2: Testing and
;; =============================================================================
(display "\n>>> Section 2: Testing and\n")
(test "and empty" '(and) #t)
(test "and single" '(and 1) 1)
(test "and multiple" '(and 1 2 3) '(if 1 (if 2 3 #f) #f))

;; =============================================================================
;; Section 3: Testing or
;; =============================================================================
(display "\n>>> Section 3: Testing or\n")
(test "or empty" '(or) #f)
(test "or single" '(or 1) 1)
(test "or multiple" '(or 1 2 3) '(let ((tmp 1)) (if tmp tmp (let ((tmp 2)) (if tmp tmp 3)))))

;; =============================================================================
;; Section 4: Testing case
;; =============================================================================
(display "\n>>> Section 4: Testing case\n")
(test "case basic" '(case 1 ((1) 'one) ((2) 'two)) '(let ((tmp 1)) (if (memv tmp '(1)) 'one (if (memv tmp '(2)) 'two (begin)))))
(test "case else" '(case 1 ((2) 'two) (else 'other)) '(let ((tmp 1)) (if (memv tmp '(2)) 'two 'other)))

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
