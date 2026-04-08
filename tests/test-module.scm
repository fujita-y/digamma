(copy-environment-variables! (system-environment) (current-environment) '(lookup-module))

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (core-eval (macroexpand expr) (current-environment))))
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

(define-module (math utils)
  (export add multiply)
  (begin
    (define (add x y) (+ x y))
    (define (multiply x y) (* x y))
    (define (internal-helper x) (* x 2))))

;; Test that module was registered
(let ((module-info (lookup-module '(math utils))))
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

(import-module (math utils))

;; Test imported functions
(test "Use imported add function"
      '(add 3 5)
      8)

(test "Use imported multiply function"
      '(multiply 4 6)
      24)

;; =============================================================================
;; Section 3: Renamed exports
;; =============================================================================
(display "\n>>> Section 3: Renamed exports\n")

(define-module (ops)
  (export (rename double-val double))
  (begin
    (define (double-val x) (* x 2))))

(import-module (ops))

(test "Use renamed export"
      '(double 5)
      10)

;; =============================================================================
;; Section 4: Import modifiers
;; =============================================================================
(display "\n>>> Section 4: Import modifiers\n")

;; Test prefix modifier
(import-module (prefix (math utils) m:))

(test "Use prefixed import"
      '(m:add 10 20)
      30)

;; Define module for testing other modifiers
(define-module (test-mod)
  (export a b c d)
  (begin
    (define (a) 'a-val)
    (define (b) 'b-val)
    (define (c) 'c-val)
    (define (d) 'd-val)))

;; Test only modifier
(import-module (only (test-mod) a c))

(test "Use only imported a"
      '(a)
      'a-val)

(test "Use only imported c"
      '(c)
      'c-val)

;; Test except modifier
(define-module (test-mod2)
  (export x y z)
  (begin
    (define (x) 'x-val)
    (define (y) 'y-val)
    (define (z) 'z-val)))

(import-module (except (test-mod2) y))

(test "Use except imported x"
      '(x)
      'x-val)

(test "Use except imported z"
      '(z)
      'z-val)

;; Test rename modifier
(import-module (rename (math utils) (add plus) (multiply times)))

(test "Use rename imported plus"
      '(plus 7 8)
      15)

(test "Use rename imported times"
      '(times 3 9)
      27)

;; =============================================================================
;; Section 5: Module with imports
;; =============================================================================
(display "\n>>> Section 5: Module with imports\n")

(define-module (composite)
  (export combined)
  (import (math utils))
  (begin
    (define (combined x y)
      (multiply (add x y) 2))))

(import-module (composite))

(test "Use function that depends on imported functions"
      '(combined 3 4)
      14)

;; =============================================================================
;; Section 6: R7RS Use Cases
;; =============================================================================
(display "\n>>> Section 6: R7RS Use Cases\n")

;; Use Case 1: Information Hiding (Stack Implementation)
;; Exports only push!, pop!, and make-stack, hiding the internal list structure
(define-module (data stack)
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

(import-module (data stack))

(test "Stack usage"
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
(define-module (geometry primitives)
  (export point make-point point-x point-y)
  (begin
    (define (point x y) (cons x y))
    (define make-point point)
    (define point-x car)
    (define point-y cdr)))

(define-module (geometry colors)
  (export color make-color)
  (begin
    (define (color r g b) (vector r g b))
    (define make-color color)))

;; The canvas library re-exports everything from primitives and colors,
;; plus adds its own 'draw' function.
(define-module (geometry canvas)
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

(import-module (geometry canvas))

(test "Aggregated library usage (Point)"
      '(point-x (make-point 10 20))
      10)

(test "Aggregated library usage (Color)"
      '(make-color 255 0 0)
      '#(255 0 0))

(test "Aggregated library usage (Own Export)"
      '(draw (make-point 0 0))
      "Draw Point")


;; Use Case 3: Renaming at multiple levels
;; Converting from "internal naming" to "public API naming"
(define-module (internal calc)
  (export calc-sum calc-diff calc-prod calc-quot)
  (begin
    (define (calc-sum a b) (+ a b))
    (define (calc-diff a b) (- a b))
    (define (calc-prod a b) (* a b))
    (define (calc-quot a b) (/ a b))))

(define-module (api math)
  (export (rename calc-sum plus)
          (rename calc-diff minus)
          (rename calc-prod mul)
          (rename calc-quot div))
  (import (internal calc))
  (begin)) ;; Pure wrapper/renamer

(import-module (prefix (api math) math:))

(test "Renamed API usage"
      '(list (math:plus 10 2) (math:minus 10 2))
      '(12 8))

;; =============================================================================
;; Section 7: Macro Export and Import
;; =============================================================================
(display "\n>>> Section 7: Macro Export and Import\n")

(define-module (test macro-export)
  (export when-test)
  (begin
    (define-syntax when-test
      (syntax-rules ()
        ((_ test expr ...)
         (if test
             (begin expr ...)
             #f))))))

(import-module (test macro-export))

(test "Use imported macro (true case)"
      '(let ((x 0))
         (when-test #t (set! x 1))
         x)
      1)

(test "Use imported macro (false case)"
      '(let ((x 0))
         (when-test #f (set! x 1))
         x)
      0)

;; =============================================================================

(define-module (test syntax-case-export)
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

(import-module (test syntax-case-export))

(test "Use imported syntax-case macro"
      '(my-or #f 1 2)
      1)

;; =============================================================================

(define-module (test helper-export)
  (export param-macro)
  (begin
    (define (helper x)
      (list 'helper x))
    (define-syntax param-macro
      (lambda (x)
        (syntax-case x ()
          ((_ val)
           (syntax (helper val))))))))

(import-module (test helper-export))

(test "Use macro using helper"
      '(param-macro 10)
      '(helper 10))

;; =============================================================================

(define-module (struct-def)
  (export define-struct)
  (begin
    (define-syntax define-struct
      (lambda (x)
        (syntax-case x ()
          ((_ name (field ...))
           (with-syntax ((make-name (datum->syntax (syntax name)
                                                   (string->symbol
                                                    (string-append "make-"
                                                                   (symbol->string
                                                                    (syntax->datum (syntax name)))))))
                         (name? (datum->syntax (syntax name)
                                               (string->symbol
                                                (string-append (symbol->string
                                                                (syntax->datum (syntax name))) "?")))))
             (syntax (begin
                       (define (make-name field ...) (list (quote name) field ...))
                       (define (name? obj) (and (pair? obj) (eq? (car obj) (quote name)))))))))))))

(define-module (struct-user)
  (export make-point point?)
  (import (struct-def))
  (begin
    (define-struct point (x y))))

(import-module (struct-user))

(test "Use imported generated macro 1" 
      '(point? 8)
      #f)

(test "Use imported generated macro 2" 
      '(make-point 3 4)
      '(point 3 4))

;; =============================================================================
;; Section 8: Additional R7RS Use Cases
;; =============================================================================
(display "\n>>> Section 8: Additional R7RS Use Cases\n")

;; Use Case 8.1: Set Implementation
;; Uses a list to store unique elements.
(define-module (data set)
  (export make-set set-add set-member? set-union)
  (begin
    (define (make-set) '())
    (define (set-member? s x)
      (if (memq x s) #t #f))
    (define (set-add s x)
      (if (set-member? s x) s (cons x s)))
    (define (set-union s1 s2)
      (if (null? s1)
          s2
          (set-union (cdr s1) (set-add s2 (car s1)))))))

(import-module (data set))

(test "Set usage"
      '(let* ((s1 (set-add (set-add (make-set) 1) 2))
              (s2 (set-add (make-set) 3))
              (s3 (set-union s1 s2)))
         (list (set-member? s3 1)
               (set-member? s3 2)
               (set-member? s3 3)
               (set-member? s3 4)))
      '(#t #t #t #f))

;; Use Case 8.2: Grid System
;; Uses a 1D vector to simulate a 2D grid.
(define-module (data grid)
  (export make-grid grid-ref grid-set! grid-rows grid-cols)
  (begin
    (define (make-grid rows cols init)
      (vector rows cols (make-vector (* rows cols) init)))
    (define (grid-rows g) (vector-ref g 0))
    (define (grid-cols g) (vector-ref g 1))
    (define (grid-ref g r c)
      (let ((data (vector-ref g 2))
            (cols (grid-cols g)))
        (vector-ref data (+ (* r cols) c))))
    (define (grid-set! g r c val)
      (let ((data (vector-ref g 2))
            (cols (grid-cols g)))
        (vector-set! data (+ (* r cols) c) val)))))

(import-module (data grid))

(test "Grid usage"
      '(let ((g (make-grid 3 3 0)))
         (grid-set! g 1 1 5)
         (list (grid-rows g) (grid-cols g) (grid-ref g 1 1) (grid-ref g 0 0)))
      '(3 3 5 0))

;; Use Case 8.3: Logger System
;; Uses internal state (closure) to manage logs.
(define-module (system logger)
  (export log-msg get-log clear-log)
  (begin
    (define logs '())
    (define (log-msg msg)
      (set! logs (cons msg logs)))
    (define (get-log)
      (reverse logs))
    (define (clear-log)
      (set! logs '()))))

(import-module (system logger))

(test "Logger usage"
      '(begin
         (clear-log)
         (log-msg "Error!")
         (log-msg "Info")
         (get-log))
      '("Error!" "Info"))

;; =============================================================================
;; Section 9: Advanced Macro Tests
;; =============================================================================
(display "\n>>> Section 9: Advanced Macro Tests\n")

;; Use Case 9.1: Macro Composition (Getter/Setter Generator)
;; Defines a macro that generates other defines and uses an imported macro/function.
(define-module (macro tools)
  (export define-getter-setter)
  (begin
    (define-syntax define-getter-setter
      (syntax-rules ()
        ((_ container index getter setter)
         (begin
           (define (getter) (vector-ref container index))
           (define (setter val) (vector-set! container index val))))))))

(import-module (macro tools))

(test "Macro composition usage"
      '(begin
         (define my-vec (vector 10 20))
         (define-getter-setter my-vec 0 get-first set-first!)
         (set-first! 100)
         (list (get-first) (vector-ref my-vec 0)))
      '(100 100))

;; Use Case 9.2: Hygiene Test
;; Ensures macros don't capture local variables.
(define-module (macro hygiene)
  (export check-hygiene)
  (begin
    (define internal-val 42)
    (define-syntax check-hygiene
      (syntax-rules ()
        ((_ expr)
         (let ((internal-val 99))
           (list internal-val expr))))))) ;; Should return (99 <expr>)

(import-module (macro hygiene))

(test "Hygiene usage 1"
      '(let ((internal-val 0))
         (check-hygiene internal-val))
      '(99 0))

(test "Hygiene usage 2"
      '(begin 
         (define internal-val -1)
         (check-hygiene internal-val))
      '(99 -1))

;; =============================================================================
;; Summary
;; =============================================================================
(newline)
(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (begin (display "ALL TESTS PASSED.\n") (exit 0))
    (begin (display "FAILED ") (display *fail-count*) (display " TESTS.\n") (exit 1)))

#|
./build/nanos --boot boot/core.ir --script tests/test-module.scm
|#
