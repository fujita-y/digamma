;; test_common.scm
;; Test suite for common.scm utilities.

(load "../common.scm")

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name output expected)
  (if (equal? output expected)
      (begin 
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS: ") (display name) (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write output) (newline))))

;; =============================================================================
;; Predicates
;; =============================================================================
(display "\n>>> Predicates\n")

(test "proper-list? (1 2 3)" (proper-list? '(1 2 3)) #t)
(test "proper-list? ()" (proper-list? '()) #t)
(test "proper-list? (1 . 2)" (proper-list? '(1 . 2)) #f)
(test "proper-list? 1" (proper-list? 1) #f)

(test "any (even? (1 2 3))" (any even? '(1 2 3)) #t)
(test "any (even? (1 3 5))" (any even? '(1 3 5)) #f)
(test "any (even? ())" (any even? '()) #f)

(test "every (even? (2 4 6))" (every even? '(2 4 6)) #t)
(test "every (even? (2 3 6))" (every even? '(2 3 6)) #f)
(test "every (even? ())" (every even? '()) #t)

;; =============================================================================
;; List Transformation & Iteration
;; =============================================================================
(display "\n>>> List Transformation\n")

(test "filter (even? (1 2 3 4))" (filter even? '(1 2 3 4)) '(2 4))
(test "filter (odd? (1 2 3 4))" (filter odd? '(1 2 3 4)) '(1 3))
(test "filter (even? ())" (filter even? '()) '())

(test "fold (+ 0 (1 2 3))" (fold + 0 '(1 2 3)) 6)
(test "fold (cons '() (1 2 3))" (fold cons '() '(1 2 3)) '(3 2 1))

(test "iota 3" (iota 3) '(0 1 2))
(test "iota 0" (iota 0) '())

(let-values (((in out) (partition even? '(1 2 3 4 5))))
  (test "partition (even? (1 2 3 4 5)) - in" in '(2 4))
  (test "partition (even? (1 2 3 4 5)) - out" out '(1 3 5)))

(test "take (1 2 3 4) 2" (take-elements '(1 2 3 4) 2) '(1 2))
(test "take (1 2 3 4) 0" (take-elements '(1 2 3 4) 0) '())
(test "take () 2" (take-elements '() 2) '())

(test "drop (1 2 3 4) 2" (drop-elements '(1 2 3 4) 2) '(3 4))
(test "drop (1 2 3 4) 0" (drop-elements '(1 2 3 4) 0) '(1 2 3 4))
(test "drop () 2" (drop-elements '() 2) '())

(test "string-join (\"a\" \"b\" \"c\") \",\"" (string-join '("a" "b" "c") ",") "a,b,c")
(test "string-join (\"a\") \",\"" (string-join '("a") ",") "a")
(test "string-join () \",\"" (string-join '() ",") "")

(test "map-improper (1 2 . 3)" (map-improper (lambda (x) (* x 2)) '(1 2 . 3)) '(2 4 . 6))
(test "map-improper (1 2 3)" (map-improper (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))

(test "flatten-begins ((begin 1 2) 3 (begin (begin 4 5) 6))" 
      (flatten-begins '((begin 1 2) 3 (begin (begin 4 5) 6)))
      '(1 2 3 4 5 6))

(test "remove-from-list (1 2 3 4 5) (2 4)" (remove-from-list '(1 2 3 4 5) '(2 4)) '(1 3 5))

(test "delete 2 (1 2 3 2 4)" (delete 2 '(1 2 3 2 4)) '(1 3 4))

;; =============================================================================
;; Set Operations
;; =============================================================================
(display "\n>>> Set Operations\n")

(define (set=? s1 s2)
  (and (= (length s1) (length s2))
       (every (lambda (x) (memq x s2)) s1)))

(test "set-union (1 2) (2 3) (4)" (set=? (set-union '(1 2) '(2 3) '(4)) '(1 2 3 4)) #t)

(test "set-minus (1 2 3 4) (2 4)" (set-minus '(1 2 3 4) '(2 4)) '(1 3))

;; =============================================================================
;; Patterns
;; =============================================================================
(display "\n>>> Patterns\n")

(test "match-rec-pattern (let ((f ...)) (set! f (lambda ...)))"
      (match-rec-pattern '(let ((f #f)) (set! f (lambda (x) x)) f))
      #t)

(test "match-rec-pattern (let ((f ...)) 123)"
      (match-rec-pattern '(let ((f #f)) 123))
      #f)

(test "match-rec-pattern simple tail recursion true match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (if (eq? n 0) 1 (<name> (- n 1))))) <name>))
      #t)

(test "match-rec-pattern non-tail recursion false match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (list 2 (<name> 9) n))) <name>))
      #f)

(test "match-rec-pattern nested lambda false match"
      (match-rec-pattern '(let ((<name> #f)) (set! <name> (lambda (n) (if n (lambda () (<name> 9)) 1))) <name>))
      #f)

(test "generate-temporary-symbol test"
      (let ((s1 (generate-temporary-symbol "t"))
            (s2 (generate-temporary-symbol "t")))
        (and (symbol? s1) (symbol? s2) (not (eq? s1 s2))))
      #t)

(newline)
(display "Total tests: ") (display (+ *pass-count* *fail-count*)) (newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
