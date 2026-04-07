;; test_hygiene.scm
;; Comprehensive hygiene tests for the macro expansion system.

;; --- Test Helper Functions ---

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

(define (test-no-capture name expr introduced-id)
  (let ((expanded (macroexpand expr)))
    ;; We expect something like (let ((x.1 ...)) ...) where x.1 != x
    (define (find-id e id)
      (cond ((eq? e id) #t)
            ((pair? e) (or (find-id (car e) id) (find-id (cdr e) id)))
            (else #f)))
    (if (find-id expanded introduced-id)
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name)
          (display " (Identifier '") (display introduced-id) (display "' was captured)")
          (newline))
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline)))))

;; =============================================================================
;; Section 1: Basic Variable Capture
;; =============================================================================
(display "\n>>> Section 1: Basic Variable Capture\n")

;; The swap! macro introduces 'tmp'.
(macroexpand '(define-syntax swap!
                (syntax-rules ()
                  ((_ a b)
                   (let ((tmp a))
                     (set! a b)
                     (set! b tmp))))))

(test-no-capture "swap! avoids capturing user's 'tmp'"
                 '(let ((tmp 1) (other 2)) (swap! tmp other))
                 'tmp)

(macroexpand '(define-syntax capture-test
                (syntax-rules ()
                  ((_ x) (let ((y 1)) x)))))

(test "capture-test preserves outer 'y'"
      '(let ((y 2)) (capture-test y))
      '(let ((y 2)) (let ((y 1)) y)))

;; =============================================================================
;; Section 2: Shadowing Global Operators
;; =============================================================================
(display "\n>>> Section 2: Shadowing Global Operators\n")

;; A macro that uses 'list' should use the global 'list' even if shadowed locally.
(macroexpand '(define-syntax make-list
                (syntax-rules ()
                  ((_ x y) (list x y)))))

(test "Macro uses global 'list' despite local shadowing"
      '(let ((list (lambda (a b) 'captured)))
         (make-list 1 2))
      '(let ((list (lambda (a b) 'captured)))
         (list 1 2)))

;; Same for 'if'
(macroexpand '(define-syntax my-if
                (syntax-rules ()
                  ((_ t a b) (if t a b)))))

(test "Macro uses global 'if' despite local shadowing"
      '(let ((if (lambda (t a b) 'captured)))
         (my-if #t 1 2))
      '(let ((if (lambda (t a b) 'captured)))
         (if #t 1 2)))

;; =============================================================================
;; Section 3: Nested Macros (The Blue/Red Macro)
;; =============================================================================
(display "\n>>> Section 3: Nested Macros (The Blue/Red Macro)\n")

;; Al Petrofsky's blue macro test.
(macroexpand '(define-syntax blue
                (syntax-rules ()
                  ((blue x)
                   (let-syntax ((red (syntax-rules ()
                                       ((red y) (list x y)))))
                     (red 'z))))))

(test "Blue/Red nested macro hygiene"
      '(let ((list (lambda (x y) 'captured)))
         (blue 'w))
      '(let ((list (lambda (x y) 'captured)))
         (list 'w 'z)))

;; =============================================================================
;; Section 4: Local Macros and Scoping
;; =============================================================================
(display "\n>>> Section 4: Local Macros and Scoping\n")

(test "Local macro captures variable at definition site (Chibi test)"
      '(let ((x 1))
         (let-syntax ((get-x (syntax-rules () ((_) x))))
           (let ((x 2))
             (get-x))))
      '(let ((x 1)) (let ((x 2)) x)))

;; =============================================================================
;; Section 5: Shadowing Core Forms as Macros
;; =============================================================================
(display "\n>>> Section 5: Shadowing Core Forms as Macros\n")

;; What if we shadow 'lambda' with a macro?
(macroexpand '(define-syntax my-lambda
                (syntax-rules ()
                  ((_ (v) body) (lambda (v) body)))))

(test "my-lambda uses core lambda even if 'lambda' is a local macro"
      '(let-syntax ((lambda (syntax-rules () ((_ args body) 'captured))))
         ((my-lambda (x) x) 1))
      '((lambda (x) x) 1))

;; =============================================================================
;; Section 6: Identifier Macros
;; =============================================================================
(display "\n>>> Section 6: Identifier Macros\n")

(environment-macro-set! 'it (lambda (expr) 'expanded-it))

(test "Identifier macro 'it' is shadowed by local binding"
      '(let ((it 1)) it)
      '(let ((it 1)) it))

(test "Identifier macro 'it' expands when not shadowed"
      '(let ((x 1)) it)
      '(let ((x 1)) expanded-it))

;; =============================================================================
;; Section 7: Integrated Components (syntax-case + quasisyntax + quasiquote)
;; =============================================================================
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

(test "integrated-qq simple unquote"
      '(integrated-qq 42)
      '(let ((tmp 42)) (list 'result tmp)))

;; Test 2: syntax-case + quasisyntax + quasiquote splicing
(macroexpand
 '(define-syntax integrated-splice
    (lambda (x)
      (syntax-case x ()
        ((_ (vals ...))
         (quasisyntax
          (let ((tmp (list (unsyntax-splicing (syntax (vals ...))))))
            `(items ,@tmp end))))))))

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
