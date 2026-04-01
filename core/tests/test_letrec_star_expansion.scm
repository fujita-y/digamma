;; test_letrec_star_expansion.scm
(if (not (undefined? load)) (load "../core.scm"))

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

(display "Testing letrec* expansion...\n")

;; letrec* should now be preserved as a core form, not desugared to let/set!
(let* ((expr '(letrec* ((a 1) (b (+ a 1))) (list a b)))
       (expanded (macroexpand expr 'strip)))
  (display "Expanded: ") (write expanded) (newline)
  (test "letrec* preserved as core form"
        expanded
        '(letrec* ((a 1) (b (+ a 1)))
           (list a b))))

;; Internal defines should now expand to letrec* core form
(let* ((expr '(let () (define (f x) (if (= x 0) 1 (* x (f (- x 1))))) (f 5)))
       (expanded (macroexpand expr 'strip)))
  (display "Expanded internal define: ") (write expanded) (newline)
  (test "internal define expansion to letrec*"
        expanded
        '(let ()
           (letrec* ((f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))))
             (f 5)))))

(display "All letrec* expansion tests done.\n")

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
