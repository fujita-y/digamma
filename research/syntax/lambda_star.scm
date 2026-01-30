;; Helper to decompose dotted/improper lists into a flat list of identifiers
;; and then call the original lambda* logic.
(define-syntax lambda*-helper
  (syntax-rules ()
    ;; Case 1: Reached the end of a dotted pair (the 'rest' variable)
    [(_ (h . t) (id ...) body)
     (lambda*-helper t (id ... h) body)]
    ;; Case 2: The tail is an identifier (improper list end) or empty
    [(_ rest (id ...) (b0 b1 ...))
     (lambda args
       (let-ids args (id ... rest) b0 b1 ...))]))

;; Sequential binding helper with O(N) traversal optimization
(define-syntax let-ids
  (syntax-rules ()
    ;; Base case: 1 identifier (the 'rest' or last one)
    ;; We bind the last identifier to the remaining list.
    [(_ ls (last) b0 b1 ...)
     (let ([last ls]) b0 b1 ...)]
    ;; Recursive step
    ;; We bind the current head to id0, and effectively peel off the cdr to a temp variable 'next'.
    ;; 'next' is then passed to the recursive call, avoiding nested (cdr (cdr ...)) chains.
    [(_ ls (id0 id1 ...) b0 b1 ...)
     (let ([id0 (car ls)]
           [next (cdr ls)])
       (let-ids next (id1 ...) b0 b1 ...))]))

;; Main macro
(define-syntax lambda*
  (syntax-rules ()
    ;; Handle standard proper lists: (lambda* (x y) ...)
    [(_ (id ...) b0 b1 ...)
     (lambda (id ...) b0 b1 ...)]
    ;; Handle improper/dotted lists: (lambda* (x . y) ...)
    [(_ (h . t) b0 b1 ...)
     (lambda*-helper (h . t) () (b0 b1 ...))]
    ;; Handle single rest argument: (lambda* args ...)
    [(_ rest b0 b1 ...)
     (lambda rest b0 b1 ...)]))
