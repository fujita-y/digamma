;; test_module_macroexpand.scm
;; Test suite for define-module and import-module in macroexpand.scm

(load "./macroexpand.scm")

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

(define (test-eval name expr expected)
  (let ((result (eval (macroexpand expr) (interaction-environment))))
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
;; Section 1: Simple module definition
;; =============================================================================
(display "\n>>> Section 1: Simple module definition\n")

(test "define-module expansion"
      '(define-module (math utils)
         (export add multiply)
         (begin
           (define (add x y) (+ x y))
           (define (multiply x y) (* x y))
           (define (internal-helper x) (* x 2))))
      ''defined)

;; Test that module was registered
(let ((module-info (mc:lookup-module '(math utils))))
  (if module-info
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: Module registered in registry\n"))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: Module not found in registry\n"))))

;; =============================================================================
;; Section 2: Import module and use exported functions
;; =============================================================================
(display "\n>>> Section 2: Import module and use exported functions\n")

(test "import-module expansion"
      '(import-module (math utils))
      ''imported)

;; Test imported functions
(test-eval "Use imported add function"
           '(add 3 5)
           8)

(test-eval "Use imported multiply function"
           '(multiply 4 6)
           24)

;; =============================================================================
;; Section 3: Renamed exports
;; =============================================================================
(display "\n>>> Section 3: Renamed exports\n")

(test "define-module with renamed export"
      '(define-module (ops)
         (export (rename double-val double))
         (begin
           (define (double-val x) (* x 2))))
      ''defined)

(macroexpand '(import-module (ops)))

(test-eval "Use renamed export"
           '(double 5)
           10)

;; =============================================================================
;; Section 4: Import modifiers
;; =============================================================================
(display "\n>>> Section 4: Import modifiers\n")

;; Test prefix modifier
(test "import-module with prefix"
      '(import-module (prefix (math utils) m:))
      ''imported)

(test-eval "Use prefixed import"
           '(m:add 10 20)
           30)

;; Define module for testing other modifiers
(macroexpand '(define-module (test-mod)
                (export a b c d)
                (begin
                  (define (a) 'a-val)
                  (define (b) 'b-val)
                  (define (c) 'c-val)
                  (define (d) 'd-val))))

;; Test only modifier
(macroexpand '(import-module (only (test-mod) a c)))

(test-eval "Use only imported a"
           '(a)
           'a-val)

(test-eval "Use only imported c"
           '(c)
           'c-val)

;; Test except modifier
(macroexpand '(define-module (test-mod2)
                (export x y z)
                (begin
                  (define (x) 'x-val)
                  (define (y) 'y-val)
                  (define (z) 'z-val))))

(macroexpand '(import-module (except (test-mod2) y)))

(test-eval "Use except imported x"
           '(x)
           'x-val)

(test-eval "Use except imported z"
           '(z)
           'z-val)

;; Test rename modifier
(macroexpand '(import-module (rename (math utils) (add plus) (multiply times))))

(test-eval "Use rename imported plus"
           '(plus 7 8)
           15)

(test-eval "Use rename imported times"
           '(times 3 9)
           27)

;; =============================================================================
;; Section 5: Module with imports
;; =============================================================================
(display "\n>>> Section 5: Module with imports\n")

(macroexpand '(define-module (composite)
                (export combined)
                (import (math utils))
                (begin
                  (define (combined x y)
                    (multiply (add x y) 2)))))

(macroexpand '(import-module (composite)))

(test-eval "Use function that depends on imported functions"
           '(combined 3 4)
           14)

;; =============================================================================
;; Section 6: R7RS Use Cases
;; =============================================================================
(display "\n>>> Section 6: R7RS Use Cases\n")

;; Use Case 1: Information Hiding (Stack Implementation)
;; Exports only push!, pop!, and make-stack, hiding the internal list structure
(test "Define stack library"
      '(define-module (data stack)
         (export make-stack push! pop! stack-empty?)
         (begin
           (define (make-stack) (vector '()))
           (define (push! s val)
             (vector-set! s 0 (cons val (vector-ref s 0))))
           (define (pop! s)
             (let ((lst (vector-ref s 0)))
               (if (null? lst)
                   (error "Stack underflow")
                   (begin
                     (vector-set! s 0 (cdr lst))
                     (car lst)))))
           (define (stack-empty? s)
             (null? (vector-ref s 0)))))
      ''defined)

(macroexpand '(import-module (data stack)))

(test-eval "Stack usage"
           '(let ((s (make-stack)))
              (push! s 1)
              (push! s 2)
              (let* ((v1 (pop! s))
                     (dummy (push! s 3))
                     (v2 (pop! s))
                     (v3 (pop! s))
                     (empty (stack-empty? s)))
                (list v1 v2 v3 empty)))
           '(2 3 1 #t))


;; Use Case 2: Library Aggregation (Re-exporting)
;; A library that groups others for convenience
(test "Define geometry primitives"
      '(define-module (geometry primitives)
         (export point make-point point-x point-y)
         (begin
           (define (point x y) (cons x y))
           (define make-point point)
           (define point-x car)
           (define point-y cdr)))
      ''defined)

(test "Define geometry colors"
      '(define-module (geometry colors)
         (export color make-color)
         (begin
           (define (color r g b) (vector r g b))
           (define make-color color)))
      ''defined)

;; The canvas library re-exports everything from primitives and colors,
;; plus adds its own 'draw' function.
(test "Define geometry canvas (aggregator)"
      '(define-module (geometry canvas)
         ;; Re-export specific items from imports
         (export point make-point point-x point-y)
         (export color make-color)
         (export draw)
         (import (geometry primitives))
         (import (geometry colors))
         (begin
           (define (draw thing)
             (cond ((pair? thing) "Draw Point")
                   ((vector? thing) "Draw Color")
                   (else "Unknown")))))
      ''defined)

(macroexpand '(import-module (geometry canvas)))

(test-eval "Aggregated library usage (Point)"
           '(point-x (make-point 10 20))
           10)

(test-eval "Aggregated library usage (Color)"
           '(make-color 255 0 0)
           '#(255 0 0))

(test-eval "Aggregated library usage (Own Export)"
           '(draw (make-point 0 0))
           "Draw Point")


;; Use Case 3: Renaming at multiple levels
;; Converting from "internal naming" to "public API naming"
(test "Define calculator internal"
      '(define-module (internal calc)
         (export calc-sum calc-diff calc-prod calc-quot)
         (begin
           (define (calc-sum a b) (+ a b))
           (define (calc-diff a b) (- a b))
           (define (calc-prod a b) (* a b))
           (define (calc-quot a b) (/ a b))))
      ''defined)

(test "Define calculator API (renaming)"
      '(define-module (api math)
         (export (rename calc-sum plus)
                 (rename calc-diff minus)
                 (rename calc-prod mul)
                 (rename calc-quot div))
         (import (internal calc))
         (begin)) ;; Pure wrapper/renamer
      ''defined)

(macroexpand '(import-module (prefix (api math) math:)))

(test-eval "Renamed API usage"
           '(list (math:plus 10 2) (math:minus 10 2))
           '(12 8))

;; =============================================================================
;; Section 7: Macro Export and Import
;; =============================================================================
(display "\n>>> Section 7: Macro Export and Import\n")

(test "Define module exporting a macro"
      '(define-module (test macro-export)
         (export when-test)
         (begin
           (define-syntax when-test
             (syntax-rules ()
               ((_ test expr ...)
                (if test
                    (begin expr ...)
                    #f))))))
      ''defined)

(macroexpand '(import-module (test macro-export)))

(test-eval "Use imported macro (true case)"
           '(let ((x 0))
              (when-test #t (set! x 1))
              x)
           1)

(test-eval "Use imported macro (false case)"
           '(let ((x 0))
              (when-test #f (set! x 1))
              x)
           0)

;; =============================================================================

(test "Define module exporting syntax-case macro"
      '(define-module (test syntax-case-export)
         (export my-or)
         (begin
           (define-syntax my-or
             (lambda (x)
               (syntax-case x ()
                 ((_) (syntax #f))
                 ((_ e) (syntax e))
                 ((_ e1 e2 e3 ...)
                  (syntax (let ((t e1))
                            (if t t (my-or e2 e3 ...))))))))))
      ''defined)

(macroexpand '(import-module (test syntax-case-export)))

(test-eval "Use imported syntax-case macro"
           '(my-or #f 1 2)
           1)

;; =============================================================================

(test "Define module with macro using helper"
      '(define-module (test helper-export)
         (export param-macro)
         (begin
           (define (helper x)
             (list 'helper x))
           (define-syntax param-macro
             (lambda (x)
               (syntax-case x ()
                 ((_ val)
                  (syntax (helper val))))))))
      ''defined)

(macroexpand '(import-module (test helper-export)))

(test-eval "Use macro using helper"
           '(param-macro 10)
           '(helper 10))

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
