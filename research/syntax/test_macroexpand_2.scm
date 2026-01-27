(load "./macroexpand.scm")

(define *pass-count* 0)
(define *fail-count* 0)


(define (test name expr expected)
  (let ((result (macroexpand expr 'strip)))
    (if (equal? result expected)
        (begin
          (display (format "PASS: ~a\n" name))
          (set! *pass-count* (+ *pass-count* 1)))
        (begin
          (display (format "FAIL: ~a\n" name))
          (display (format "  Expected: ~a\n" expected))
          (display (format "  Got:      ~a\n" result))
          (set! *fail-count* (+ *fail-count* 1))))))

(display "Testing cond...\n")
(test "cond basic" '(cond (#t 1) (else 2)) '(if #t 1 2))
(test "cond multiple" '(cond (#f 1) (#t 2) (else 3)) '(if #f 1 (if #t 2 3)))
(test "cond arrow" '(cond (1 => (lambda (x) x)) (else 2)) '(let ((tmp 1)) (if tmp ((lambda (x) x) tmp) 2)))

(display "\nTesting and...\n")
(test "and empty" '(and) #t)
(test "and single" '(and 1) 1)
(test "and multiple" '(and 1 2 3) '(if 1 (if 2 3 #f) #f))

(display "\nTesting or...\n")
(test "or empty" '(or) #f)
(test "or single" '(or 1) 1)
(test "or multiple" '(or 1 2 3) '(let ((tmp 1)) (if tmp tmp (let ((tmp 2)) (if tmp tmp 3)))))

(display "\nTesting case...\n")
(test "case basic" '(case 1 ((1) 'one) ((2) 'two)) '(let ((tmp 1)) (if (memv tmp '(1)) 'one (if (memv tmp '(2)) 'two (begin)))))
(test "case else" '(case 1 ((2) 'two) (else 'other)) '(let ((tmp 1)) (if (memv tmp '(2)) 'two 'other)))

(display (format "\nAll tests completed. Passes: ~a, Fails: ~a\n" *pass-count* *fail-count*))
