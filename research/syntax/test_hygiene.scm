;; test_hygiene.scm
;; Comprehensive hygiene tests for the macro expansion system.

(load "./macroexpand.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

;; Standard structural test (stripped)
(define (test expected expr msg)
  (let ((expanded (macroexpand expr 'strip)))
    (if (equal? expected expanded)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ")
          (display msg)
          (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ")
          (display msg)
          (newline)
          (display "  Expected: ")
          (write expected)
          (newline)
          (display "  Actual:   ")
          (write expanded)
          (newline)))))

;; Verify hygiene by ensuring an introduced identifier is NOT eq to a given one.
(define (test-no-capture expr introduced-id msg)
  (let ((expanded (macroexpand expr)))
    ;; We expect something like (let ((x.1 ...)) ...) where x.1 != x
    (define (find-id e id)
      (cond ((eq? e id) #t)
            ((pair? e) (or (find-id (car e) id) (find-id (cdr e) id)))
            (else #f)))
    (if (find-id expanded introduced-id)
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ")
          (display msg)
          (display " (Identifier '") (display introduced-id) (display "' was captured)")
          (newline))
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ")
          (display msg)
          (newline)))))

(display "\n>>> Section 1: Basic Variable Capture\n")

;; The swap! macro introduces 'tmp'.
(macroexpand '(define-syntax swap!
                (syntax-rules ()
                  ((_ a b)
                   (let ((tmp a))
                     (set! a b)
                     (set! b tmp))))))

(test-no-capture '(let ((tmp 1) (other 2)) (swap! tmp other))
                 'tmp
                 "swap! avoids capturing user's 'tmp'")

(macroexpand '(define-syntax capture-test
                (syntax-rules ()
                  ((_ x) (let ((y 1)) x)))))

(test '(let ((y 2)) (let ((y 1)) y))
      '(let ((y 2)) (capture-test y))
      "capture-test preserves outer 'y'")

(display "\n>>> Section 2: Shadowing Global Operators\n")

;; A macro that uses 'list' should use the global 'list' even if shadowed locally.
(macroexpand '(define-syntax make-list
                (syntax-rules ()
                  ((_ x y) (list x y)))))

(test '(let ((list (lambda (a b) 'captured)))
         (list 1 2))
      '(let ((list (lambda (a b) 'captured)))
         (make-list 1 2))
      "Macro uses global 'list' despite local shadowing")

;; Same for 'if'
(macroexpand '(define-syntax my-if
                (syntax-rules ()
                  ((_ t a b) (if t a b)))))

(test '(let ((if (lambda (t a b) 'captured)))
         (if #t 1 2))
      '(let ((if (lambda (t a b) 'captured)))
         (my-if #t 1 2))
      "Macro uses global 'if' despite local shadowing")

(display "\n>>> Section 3: Nested Macros (The Blue/Red Macro)\n")

;; Al Petrofsky's blue macro test.
(macroexpand '(define-syntax blue
                (syntax-rules ()
                  ((blue x)
                   (let-syntax ((red (syntax-rules ()
                                       ((red y) (list x y)))))
                     (red 'z))))))

(test '(let ((list (lambda (x y) 'captured)))
         (list 'w 'z))
      '(let ((list (lambda (x y) 'captured)))
         (blue 'w))
      "Blue/Red nested macro hygiene")

(display "\n>>> Section 4: Local Macros and Scoping\n")

(test '(let ((x 1)) (let ((x 2)) x))
      '(let ((x 1))
         (let-syntax ((get-x (syntax-rules () ((_) x))))
           (let ((x 2))
             (get-x))))
      "Local macro captures variable at definition site (Chibi test)")

(display "\n>>> Section 5: Shadowing Core Forms as Macros\n")

;; What if we shadow 'lambda' with a macro?
(macroexpand '(define-syntax my-lambda
                (syntax-rules ()
                  ((_ (v) body) (lambda (v) body)))))

(test '((lambda (x) x) 1)
      '(let-syntax ((lambda (syntax-rules () ((_ args body) 'captured))))
         ((my-lambda (x) x) 1))
      "my-lambda uses core lambda even if 'lambda' is a local macro")

(display "\n>>> Section 6: Identifier Macros\n")

(register-macro! 'it (lambda (expr) 'expanded-it))

(test '(let ((it 1)) it)
      '(let ((it 1)) it)
      "Identifier macro 'it' is shadowed by local binding")

(test '(let ((x 1)) expanded-it)
      '(let ((x 1)) it)
      "Identifier macro 'it' expands when not shadowed")

(display "\n>>> Section 7: Integrated Components (syntax-case + quasisyntax + quasiquote)\n")

;; Test 1: syntax-case + quasisyntax + quasiquote simple unquote
(macroexpand
 '(define-syntax integrated-qq
    (lambda (x)
      (syntax-case x ()
        ((_ val)
         (quasisyntax
          (let ((tmp (unsyntax (syntax val))))
            `(result ,tmp))))))))

(test '(let ((tmp 42)) (list 'result tmp))
      '(integrated-qq 42)
      "integrated-qq simple unquote")

;; Test 2: syntax-case + quasisyntax + quasiquote splicing
(macroexpand
 '(define-syntax integrated-splice
    (lambda (x)
      (syntax-case x ()
        ((_ (vals ...))
         (quasisyntax
          (let ((tmp (list (unsyntax-splicing (syntax (vals ...))))))
            `(items ,@tmp end))))))))

(test '(let ((tmp (list 1 2 3))) (cons 'items (append tmp (cons 'end ()))))
      '(integrated-splice (1 2 3))
      "integrated-qq splicing")

;; --- Summary ---

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL HYGIENE TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
