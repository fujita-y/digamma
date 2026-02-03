;; test_closure_analysis.scm
;; Test suite for closure escape analysis in the compiler.

(load "../compiler.scm")
(use srfi-1)

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected-escapeStatus)
  (let* ((code (cp:compile expr))
         (instructions (vector->list code))
         (make-closure-inst (find (lambda (inst) 
                                    (and (vector? inst) 
                                         (> (vector-length inst) 0)
                                         (eq? (vector-ref inst 0) 'make-closure)))
                                  instructions)))
    (if make-closure-inst
        (let* ((len (vector-length make-closure-inst))
               ;; The stack-alloc? flag is at index 4 (0-based) or -3 from end
               (status (vector-ref make-closure-inst (- len 3))))
          (if (eq? status expected-escapeStatus)
              (begin
                (set! *pass-count* (+ *pass-count* 1))
                (display "PASS: ") (display name) (newline))
              (begin
                (set! *fail-count* (+ *fail-count* 1))
                (display "FAIL: ") (display name) (newline)
                (display "  Expected: ") (write expected-escapeStatus) (newline)
                (display "  Actual:   ") (write status) (newline))))
        (begin
             (set! *fail-count* (+ *fail-count* 1))
             (display "FAIL: ") (display name) (newline)
             (display "  No make-closure instruction found.") (newline)))))

(display "\n>>> Closure Escape Analysis\n")

;; 1. Immediate application: ((lambda (x) x) 1) 
;; The lambda is immediately called and doesn't escape.
;; Expected: #t (stack allocatable)
(test "Immediate application" '((lambda (x) x) 1) #t)

;; 2. Bound to variable, only called: (let ((f (lambda (x) x))) (f 1))
;; Variable f is only used in operator position.
;; Expected: #t
(test "Bound to variable, only called" '(let ((f (lambda (x) x))) (f 1)) #t)

;; 3. Returned: (let ((f (lambda (x) x))) f)
;; Variable f is returned.
;; Expected: #f (escapes)
(test "Returned closure" '(let ((f (lambda (x) x))) f) #f)

;; 4. Passed as argument: (let ((f (lambda (x) x))) (map f '(1 2)))
;; Variable f is passed to map.
;; Expected: #f
(test "Passed as argument" '(let ((f (lambda (x) x))) (map f '(1 2))) #f)

;; 5. Stored in data structure: (let ((f (lambda (x) x))) (cons f f))
;; Variable f is in cons.
;; Expected: #f
(test "Stored in data structure" '(let ((f (lambda (x) x))) (cons f f)) #f)

;; 6. Letrec / recursive function (simple tail call pattern)
;; (let ((loop (lambda (n) (if (= n 0) 0 (loop (- n 1)))))) (loop 10))
;; Loop is only called. 
;; Expected: #t
(test "Recursive loop (letrec/named let pattern)" '(let ((loop (lambda (n) (if (= n 0) 0 (loop (- n 1)))))) (loop 10)) #t)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
