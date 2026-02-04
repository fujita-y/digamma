(load "../core.scm")

(define *pass-count* 0)
(define *fail-count* 0)

(define (check-exact name input expected)
  (let ((result (peephole-optimize input)))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  Expected: ") (write expected) (newline)
          (display "  Got:      ") (write result) (newline)))))

;; Compile-based tests are harder to exact-match due to label generation/register allocation details,
;; but we can verify the optimizer behavior directly using raw instruction lists.

;; --- 2-instruction Optimizations ---

;; Case 1: Redundant Reg Cell Ref
(check-exact "Simple redundant cell ref (raw)"
             '((reg-cell-set! rA rB) (reg-cell-ref rB rA))
             '((reg-cell-set! rA rB)))

;; Case 2: Reg Cell Ref to different reg
(check-exact "Cell ref to different register (raw)"
             '((reg-cell-set! rA rB) (reg-cell-ref rC rA))
             '((reg-cell-set! rA rB) (mov rC rB)))

;; Closure patterns
(check-exact "Closure cell redundant ref"
             '((closure-cell-set! 0 r1) (closure-cell-ref r1 0))
             '((closure-cell-set! 0 r1)))

(check-exact "Closure cell ref to mov"
             '((closure-cell-set! 0 r1) (closure-cell-ref r2 0))
             '((closure-cell-set! 0 r1) (mov r2 r1)))

(check-exact "Closure set redundant ref"
             '((closure-set! 0 r1) (closure-ref r1 0))
             '((closure-set! 0 r1)))

(check-exact "Closure ref to mov"
             '((closure-set! 0 r1) (closure-ref r2 0))
             '((closure-set! 0 r1) (mov r2 r1)))

;; Pattern: (mov A B) (mov B A) -> (mov A B)
(check-exact "2-inst Redundant Move Back"
             '((mov r1 r2) (mov r2 r1))
             '((mov r1 r2)))


;; --- 3-instruction Optimizations ---

;; Pattern 4: (reg-cell-set! rA rB) ({ops-loads} rC <any>) (reg-cell-ref rD rA)
;; -> (reg-cell-set! rA rB) ({ops-loads} rC <any>) (mov rD rB)
(check-exact "3-inst Reg Cell ref to mov"
             '((reg-cell-set! r1 r2) (const r3 10) (reg-cell-ref r4 r1))
             '((reg-cell-set! r1 r2) (const r3 10) (mov r4 r2)))

(check-exact "3-inst Reg Cell clobber source (no opt)"
             '((reg-cell-set! r1 r2) (mov r2 r3) (reg-cell-ref r4 r1))
             '((reg-cell-set! r1 r2) (mov r2 r3) (reg-cell-ref r4 r1)))

(check-exact "3-inst Reg Cell clobber cell (no opt)"
             '((reg-cell-set! r1 r2) (mov r1 r3) (reg-cell-ref r4 r1))
             '((reg-cell-set! r1 r2) (mov r1 r3) (reg-cell-ref r4 r1)))


;; Pattern 5: (closure-cell-set! IDX rB) ({ops-loads} rC <any>) (closure-cell-ref rD IDX)
(check-exact "3-inst Closure Cell ref to mov"
             '((closure-cell-set! 0 r1) (const r2 10) (closure-cell-ref r3 0))
             '((closure-cell-set! 0 r1) (const r2 10) (mov r3 r1)))

(check-exact "3-inst Closure Cell clobber source (no opt)"
             '((closure-cell-set! 0 r1) (mov r1 r2) (closure-cell-ref r3 0))
             '((closure-cell-set! 0 r1) (mov r1 r2) (closure-cell-ref r3 0)))

;; Pattern 6: (closure-set! IDX rB) ...
(check-exact "3-inst Closure ref to mov"
             '((closure-set! 0 r1) (const r2 10) (closure-ref r3 0))
             '((closure-set! 0 r1) (const r2 10) (mov r3 r1)))

(check-exact "3-inst Closure clobber source (no opt)"
             '((closure-set! 0 r1) (mov r1 r2) (closure-ref r3 0))
             '((closure-set! 0 r1) (mov r1 r2) (closure-ref r3 0)))


;; Pattern 1: Propagate Load into Move
;; (const r1 10) (mov r2 r1) (const r1 20) -> (const r2 10) (const r1 20)
(check-exact "3-inst Propagate Load"
             '((const r1 10) (mov r2 r1) (const r1 20))
             '((const r2 10) (const r1 20)))

;; Pattern 2: Propagate Move into Move
;; (mov r1 r2) (mov r3 r1) (mov r1 r4) -> (mov r3 r2) (mov r1 r4)
(check-exact "3-inst Propagate Move"
             '((mov r1 r2) (mov r3 r1) (mov r1 r4))
             '((mov r3 r2) (mov r1 r4)))

;; Pattern 3: Redundant Restore (3-inst)
;; (mov r1 r2) (const r3 10) (mov r2 r1) -> (mov r1 r2) (const r3 10)
(check-exact "3-inst Redundant Restore"
             '((mov r1 r2) (const r3 10) (mov r2 r1))
             '((mov r1 r2) (const r3 10)))

;; Pattern: Redundant Restore Chain (N-inst)
;; (mov rA rB) ... (mov rB rA)
(check-exact "N-inst Redundant Restore Chain"
             '((mov r1 r2) (const r3 10) (global-ref r4 'foo) (mov r2 r1))
             '((mov r1 r2) (const r3 10) (global-ref r4 'foo)))

(check-exact "N-inst Redundant Restore Fail (clobber A)"
             '((mov r1 r2) (const r1 10) (mov r2 r1))
             '((mov r1 r2) (const r1 10) (mov r2 r1)))

(check-exact "N-inst Redundant Restore Fail (bad op)"
             '((mov r1 r2) (call r3 0) (mov r2 r1))
             '((mov r1 r2) (call r3 0) (mov r2 r1)))


;; Self-Move Elimination
(check-exact "Self-Move Elimination"
             '((mov r1 r1))
             '())

(check-exact "Self-Move Elimination in sequence"
             '((const r1 10) (mov r1 r1) (ret))
             '((const r1 10) (ret)))

(if (= *fail-count* 0)
    (display "ALL PEEPHOLE TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " PEEPHOLE TESTS.\n")
      (exit 1)))
(newline)
