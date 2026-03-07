(load "../core.scm")

(define *pass-count* 0)
(define *fail-count* 0)

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

;; Test 1: Lift a simple let-bound lambda (using set!) with no free vars
(test "Simple lift (no free vars)"
      '(let ((f #f))
         (set! f (lambda (x) (+ x 1)))
         (f 10))
      '(begin
         (define f_ (lambda (x) (+ x 1)))
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
(test "Map example (from issue)"
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
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
