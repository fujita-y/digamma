;; test_closure.scm
;; Test suite for closure conversion and lambda lifting.

(load "closure_conversion.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

;; Helper to normalize gensyms generated during closure conversion.
;; It replaces symbols like #:_close_noname_3 with G1, G2, etc.
(define (normalize-gensyms expr)
  (let ((map '())
        (counter 0))
    (define (get-replacement sym)
      (let ((pair (assq sym map)))
        (if pair
            (cdr pair)
            (begin
              (set! counter (+ counter 1))
              (let ((new-sym (string->symbol (string-append "G" (number->string counter)))))
                (set! map (cons (cons sym new-sym) map))
                new-sym)))))
    
    (define (is-gensym? s)
      (and (symbol? s) (not (symbol-interned? s))))

    (let recur ((e expr))
      (cond ((is-gensym? e) (get-replacement e) )
            ((pair? e) (cons (recur (car e)) (recur (cdr e))))
            ((vector? e) (list->vector (map recur (vector->list e))))
            (else e)))))

(define (test name expr expected)
  (let ((result (normalize-gensyms (closure-conversion expr))))
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
;; Section 1: Basic Closure Conversion
;; =============================================================================
(display "\n>>> Section 1: Basic Closure Conversion\n")

(test "Case 0 (No capture)"
      '(define foo (lambda (lst) (map (lambda (x) (+ x 3)) lst)))
      '((define G1 (lambda (x) (+ x 3)))
        (define foo (lambda (lst) (map G1 lst)))))

(test "Case 1 (Simple capture)"
      '(define foo (lambda (val lst) (map (lambda (x) (+ x val)) lst)))
      '((define G1 (lambda (val) (lambda (x) (+ x val))))
        (define foo (lambda (val lst) (map (G1 val) lst)))))

;; =============================================================================
;; Section 2: Mutability and Boxing
;; =============================================================================
(display "\n>>> Section 2: Mutability and Boxing\n")

(test "Case 2 (Mutability)"
      '(define foo (lambda (val lst)
                   (set! val (- val 2))
                   (cons (map (lambda (x) (set! val (- val 1)) (+ x val)) lst)
                         val)))
      '((define G1 (lambda (val) (lambda (x) (begin (vector-set! val 0 (- (vector-ref val 0) 1)) (+ x (vector-ref val 0))))))
        (define foo (lambda (val lst)
                    (let ((val (vector val)))
                      (vector-set! val 0 (- (vector-ref val 0) 2))
                      (cons (map (G1 val) lst) (vector-ref val 0)))))))

;; =============================================================================
;; Section 3: Nested Lambdas
;; =============================================================================
(display "\n>>> Section 3: Nested Lambdas\n")

(test "Case 3 (Nested Lambdas)"
      '(define nesting (lambda (x) (lambda (y) (lambda (z) (+ x (+ y z))))))
      '((define G1 (lambda (x y) (lambda (z) (+ x (+ y z)))))
        (define G2 (lambda (x) (lambda (y) (G1 x y))))
        (define nesting (lambda (x) (G2 x)))))

(test "Case 4 (Multiple Args)"
      '(define multi (lambda (a) (lambda (b c) (+ a (+ b c)))))
      '((define G1 (lambda (a) (lambda (b c) (+ a (+ b c)))))
        (define multi (lambda (a) (G1 a)))))

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
