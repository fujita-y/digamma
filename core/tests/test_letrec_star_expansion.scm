;; test_letrec_star_expansion.scm
(load "../core.scm")

(define (test name output expected)
  (if (equal? output expected)
      (begin (display "PASS: ") (display name) (newline))
      (begin
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline)
        (exit 1))))

(display "Testing letrec* expansion...\n")

(let* ((expr '(letrec* ((a 1) (b (+ a 1))) (list a b)))
       (expanded (macroexpand expr 'strip)))
  (display "Expanded: ") (write expanded) (newline)
  (test "letrec* expansion to let/set!"
        expanded
        '(let ((a '*undefined*) (b '*undefined*))
           (set! a 1)
           (set! b (+ a 1))
           (list a b))))

(let* ((expr '(let () (define (f x) (if (= x 0) 1 (* x (f (- x 1))))) (f 5)))
       (expanded (macroexpand expr 'strip)))
  (display "Expanded internal define: ") (write expanded) (newline)
  (test "internal define expansion to let/set!"
        expanded
        '(let ()
           (let ((f '*undefined*))
             (set! f (lambda (x) (if (= x 0) 1 (* x (f (- x 1))))))
             (f 5)))))

(display "All letrec* expansion tests passed.\n")
