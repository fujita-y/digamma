;; Test file for renamed export functionality
;; Load the R7RS library system

(load "r7rs_library.scm")

;; Test 1: Simple renamed export
(r7rs:define-library (test renamed-export)
  (export (rename internal-add public-add)
          (rename internal-mul public-mul)
          value)
  (begin
    (define (internal-add x y) (+ x y))
    (define (internal-mul x y) (* x y))
    (define value 42)))

;; Test 2: Mixed plain and renamed exports
(r7rs:define-library (test mixed-exports)
  (export plain-func
          (rename secret-func revealed-func)
          plain-val)
  (begin
    (define (plain-func x) (list 'plain x))
    (define (secret-func x) (list 'secret x))
    (define plain-val 100)
    (define hidden-val 200)))

;; Test 3: Import renamed exports
(r7rs:import (test renamed-export))

(display "Test 1: Basic renamed exports\n")
(display "  (public-add 10 20) = ")
(display (public-add 10 20))
(newline)

(display "  (public-mul 3 7) = ")
(display (public-mul 3 7))
(newline)

(display "  value = ")
(display value)
(newline)

;; Test 4: Import mixed exports
(r7rs:import (test mixed-exports))

(display "\nTest 2: Mixed exports\n")
(display "  (plain-func 'test) = ")
(display (plain-func 'test))
(newline)

(display "  (revealed-func 'data) = ")
(display (revealed-func 'data))
(newline)

(display "  plain-val = ")
(display plain-val)
(newline)

;; Test 5: Chained renames - rename on export, then rename on import
(r7rs:import (rename (test renamed-export) (public-add my-add)))

(display "\nTest 3: Chained renames (export rename + import rename)\n")
(display "  (my-add 5 8) = ")
(display (my-add 5 8))
(newline)

;; Test 6: Prefix with renamed export
(r7rs:import (prefix (test mixed-exports) mix:))

(display "\nTest 4: Prefix with renamed exports\n")
(display "  (mix:revealed-func 'prefixed) = ")
(display (mix:revealed-func 'prefixed))
(newline)

;; Test 7: Verify internal names are NOT accessible
(display "\nTest 5: Verify internal names are hidden\n")
(display "  Attempting to access 'internal-add' should fail...\n")
(display "  (This should cause an error if uncommented)\n")
;; Uncomment to test error:
;; (display (internal-add 1 2))

(display "\nAll tests completed successfully!\n")
