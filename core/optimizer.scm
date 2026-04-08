;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; Optimizer for Scheme Core Forms
;;
;; This module performs high-level optimizations on core forms:
;; - Constant propagation and folding
;; - Dead code elimination (begin flattening, unused let-binding removal)
;; - Procedure inlining and beta-reduction
;; - If-lifting and boolean simplification
;; - Unused parameter removal in lambdas

;;=============================================================================
;; 1. Global State & Config
;;=============================================================================

(define global-env (make-eq-hashtable))
(define *inlining-depth* (make-eq-hashtable))

(define *cp0-effort-limit* 100)
(define *cp0-score-limit* 20)

;; Pure side-effect-free primitives that can be removed if their result is unused.
(define pure-primitives
  '(not boolean? boolean=? eq? eqv? equal?
    pair? cons car cdr caar cadr cdar cddr null? list? list length append reverse
    make-list list-tail list-ref memq memv member assq assv assoc
    symbol? symbol=? symbol->string string->symbol
    number? complex? real? rational? integer? exact? inexact? = < > <= >= zero?
    positive? negative? odd? even? max min + * - / abs floor ceiling truncate
    round gcd lcm numerator denominator expt sqrt number->string exact-integer-sqrt
    char? char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
    char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
    char-upper-case? char-lower-case? char->integer integer->char char-upcase char-downcase
    string? make-string string string-length string-ref string=? string<? string>?
    string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    substring string-append string->list list->string string-copy string->number
    vector? make-vector vector vector-length vector-ref vector->list list->vector
    procedure? bytevector? bytevector-length bytevector-u8-ref))

;;=============================================================================
;; 2. Analysis & Misc Utilities
;;=============================================================================

(define (unique lst)
  (if (null? lst)
      '()
      (let ((tail (unique (cdr lst))))
        (if (memq (car lst) tail)
            tail
            (cons (car lst) tail)))))

(define (flatten-params-opt params)
  (cond ((null? params) '())
        ((symbol? params) (list params))
        ((pair? params) (cons (car params) (flatten-params-opt (cdr params))))
        (else '())))

(define (compute-score expr effort)
  (if (<= effort 0)
      1000
      (cond ((not (pair? expr)) 1)
            ((eq? (car expr) 'quote) 1)
            ((eq? (car expr) 'if)
             (+ 1 (compute-score (cadr expr) (- effort 1))
                  (max (compute-score (caddr expr) (- effort 1))
                       (compute-score (cadddr expr) (- effort 1)))))
            ((eq? (car expr) 'lambda)
             (+ 5 (compute-score (cddr expr) (- effort 1))))
            ((eq? (car expr) 'letrec*)
             (+ 3 (fold (lambda (b acc) (+ acc (compute-score (cadr b) (- effort 1)))) 0 (cadr expr))
                  (fold (lambda (e acc) (+ acc (compute-score e (- effort 1)))) 0 (cddr expr))))
            (else
             (fold (lambda (e acc) (+ acc (compute-score e (- effort 1)))) 1 expr)))))

(define (small-procedure? val)
  (<= (compute-score val *cp0-effort-limit*) *cp0-score-limit*))

;; High-performance analysis with hashtable accumulator.
(define (analyze-used-vars expr)
  (let ((ht (make-eq-hashtable)))
    (let walk ((e expr) (s '()))
      (cond ((symbol? e) (unless (memq e s) (hashtable-set! ht e #t)))
            ((not (pair? e)) #f)
            ((eq? (car e) 'quote) #f)
            ((eq? (car e) 'lambda)
             (let ((new-s (append (flatten-params-opt (cadr e)) s)))
               (for-each (lambda (x) (walk x new-s)) (cddr e))))
            ((eq? (car e) 'let)
             (let ((vars (map car (cadr e))))
               (for-each (lambda (b) (walk (cadr b) s)) (cadr e))
               (for-each (lambda (x) (walk x (append vars s))) (cddr e))))
            ((eq? (car e) 'letrec*)
             (let ((vars (map car (cadr e))))
               (let ((new-s (append vars s)))
                 (for-each (lambda (b) (walk (cadr b) new-s)) (cadr e))
                 (for-each (lambda (x) (walk x new-s)) (cddr e)))))
            (else (for-each (lambda (x) (walk x s)) e))))
    (map car (hashtable->alist ht))))

(define (analyze-free-vars-optimizer expr bound-vars)
  (let ((ht (make-eq-hashtable)))
    (let walk ((e expr) (s bound-vars))
      (cond ((symbol? e) (unless (memq e s) (hashtable-set! ht e #t)))
            ((not (pair? e)) #f)
            ((eq? (car e) 'quote) #f)
            ((eq? (car e) 'lambda)
             (let ((new-s (append (flatten-params-opt (cadr e)) s)))
               (for-each (lambda (x) (walk x new-s)) (cddr e))))
            ((eq? (car e) 'let)
             (let ((vars (map car (cadr e))))
               (for-each (lambda (b) (walk (cadr b) s)) (cadr e))
               (for-each (lambda (x) (walk x (append vars s))) (cddr e))))
            ((eq? (car e) 'letrec*)
             (let ((vars (map car (cadr e))))
               (let ((new-s (append vars s)))
                 (for-each (lambda (b) (walk (cadr b) new-s)) (cadr e))
                 (for-each (lambda (x) (walk x new-s)) (cddr e)))))
            ((eq? (car e) 'if)
             (walk (cadr e) s)
             (walk (caddr e) s)
             (unless (null? (cdddr e)) (walk (cadddr e) s)))
            ((eq? (car e) 'begin) (for-each (lambda (x) (walk x s)) (cdr e)))
            ((eq? (car e) 'set!) (unless (memq (cadr e) s) (hashtable-set! ht (cadr e) #t)) (walk (caddr e) s))
            ((eq? (car e) 'define) (walk (caddr e) s))
            (else (for-each (lambda (x) (walk x s)) e))))
    (map car (hashtable->alist ht))))

(define (analyze-mutated-vars-optimizer expr)
  (let ((ht (make-eq-hashtable)))
    (let walk ((e expr) (lbound '()))
      (cond ((not (pair? e)) #f)
            ((eq? (car e) 'quote) #f)
            ((eq? (car e) 'set!)
             (unless (memq (cadr e) lbound) (hashtable-set! ht (cadr e) #t))
             (walk (caddr e) lbound))
            ((eq? (car e) 'lambda)
             (let ((new-lbound (append (flatten-params-opt (cadr e)) lbound)))
               (for-each (lambda (x) (walk x new-lbound)) (cddr e))))
            ((eq? (car e) 'let)
             (for-each (lambda (b) (walk (cadr b) lbound)) (cadr e))
             (for-each (lambda (x) (walk x lbound)) (cddr e)))
            ((eq? (car e) 'letrec*)
             (for-each (lambda (b) (walk (cadr b) lbound)) (cadr e))
             (for-each (lambda (x) (walk x lbound)) (cddr e)))
            (else (for-each (lambda (x) (walk x lbound)) e))))
    (map car (hashtable->alist ht))))

;; Combined analysis for opt-let-inner.
(define (analyze-body-combined body shadowed)
  (let ((usage (make-eq-hashtable))
        (mutated (make-eq-hashtable)))
    (let walk ((e body) (lambda-depth 0) (s shadowed) (lbound '()))
      (cond ((symbol? e)
             (unless (memq e s)
               (let ((curr (hashtable-ref usage e '(0 . #f))))
                 (hashtable-set! usage e (cons (+ (car curr) 1) (or (cdr curr) (> lambda-depth 0)))))))
            ((not (pair? e)) #f)
            ((eq? (car e) 'quote) #f)
            ((eq? (car e) 'set!)
             (unless (memq (cadr e) lbound) (hashtable-set! mutated (cadr e) #t))
             (walk (caddr e) lambda-depth s lbound))
            ((eq? (car e) 'lambda)
             (let* ((plist (flatten-params-opt (cadr e)))
                    (new-s (append plist s))
                    (new-lbound (append plist lbound)))
               (for-each (lambda (be) (walk be (+ lambda-depth 1) new-s new-lbound)) (cddr e))))
            ((eq? (car e) 'let)
             (let ((vars (map car (cadr e))))
               (for-each (lambda (b) (walk (cadr b) lambda-depth s lbound)) (cadr e))
               (for-each (lambda (be) (walk be lambda-depth (append vars s) lbound)) (cddr e))))
            ((eq? (car e) 'letrec*)
             (let ((vars (map car (cadr e))))
               (let ((new-s (append vars s)))
                 (for-each (lambda (b) (walk (cadr b) lambda-depth new-s lbound)) (cadr e))
                 (for-each (lambda (be) (walk be lambda-depth new-s lbound)) (cddr e)))))
            (else (for-each (lambda (be) (walk be lambda-depth s lbound)) e))))
    (values (map car (hashtable->alist mutated)) usage)))

(define (has-effects? expr)
  (cond ((symbol? expr) #f)
        ((not (pair? expr)) #f)
        ((eq? (car expr) 'quote) #f)
        ((eq? (car expr) 'lambda) #f)
        ((eq? (car expr) 'if)
         (or (has-effects? (cadr expr))
             (has-effects? (caddr expr))
             (and (not (null? (cdddr expr))) (has-effects? (cadddr expr)))))
        ((eq? (car expr) 'begin) (any has-effects? (cdr expr)))
        ((eq? (car expr) 'set!) #t)
        ((eq? (car expr) 'define) #t)
        ((eq? (car expr) 'let)
         (or (any (lambda (b) (has-effects? (cadr b))) (cadr expr))
             (any has-effects? (cddr expr))))
        ((eq? (car expr) 'letrec*)
         (or (any (lambda (b) (has-effects? (cadr b))) (cadr expr))
             (any has-effects? (cddr expr))))
        (else
         (not (and (memq (car expr) pure-primitives)
                   (not (any has-effects? (cdr expr))))))))

(define (safe-to-inline-val? var val body-usage bound-vars mutated-vars)
  (let ((fvars (analyze-free-vars-optimizer val '())))
    (and (not (any (lambda (v) (memq v mutated-vars)) fvars))
         (or (every (lambda (v) (or (memq v bound-vars) (memq v pure-primitives))) fvars)
             (not (cdr (hashtable-ref body-usage var '(0 . #f))))))))

;;=============================================================================
;; 3. Transformation Helpers
;;=============================================================================

(define (substitute-proc expr var val)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'lambda)
         (if (memq var (flatten-params-opt (cadr expr)))
             expr
             `(lambda ,(cadr expr) ,@(map (lambda (e) (substitute-proc e var val)) (cddr expr)))))
        ((eq? (car expr) 'letrec*)
         (let* ((vars (map car (cadr expr))))
           (if (memq var vars)
               expr
               `(letrec* ,(map (lambda (b) (list (car b) (substitute-proc (cadr b) var val))) (cadr expr))
                  ,@(map (lambda (e) (substitute-proc e var val)) (cddr expr))))))
        ((eq? (car expr) var)
         (cons val (map (lambda (e) (substitute-proc e var val)) (cdr expr))))
        (else
         (map (lambda (e) (substitute-proc e var val)) expr))))

(define (substitute-many expr mapping)
  (if (null? mapping)
      expr
      (cond ((symbol? expr) (cond ((assq expr mapping) => cdr) (else expr)))
            ((not (pair? expr)) expr)
            ((eq? (car expr) 'quote) expr)
            ((eq? (car expr) 'lambda)
             (let* ((params (cadr expr))
                    (new-mapping (filter (lambda (p) (not (memq (car p) (flatten-params-opt params)))) mapping)))
               `(lambda ,params ,@(map (lambda (e) (substitute-many e new-mapping)) (cddr expr)))))
            ((eq? (car expr) 'let)
             (let* ((bindings (cadr expr))
                    (vars (map car bindings))
                    (new-mapping (filter (lambda (p) (not (memq (car p) vars))) mapping)))
               `(let ,(map (lambda (b) (list (car b) (substitute-many (cadr b) mapping))) bindings)
                  ,@(map (lambda (e) (substitute-many e new-mapping)) (cddr expr)))))
            ((eq? (car expr) 'letrec*)
             (let* ((bindings (cadr expr))
                    (vars (map car bindings))
                    (new-mapping (filter (lambda (p) (not (memq (car p) vars))) mapping)))
               `(letrec* ,(map (lambda (b) (list (car b) (substitute-many (cadr b) new-mapping))) bindings)
                  ,@(map (lambda (e) (substitute-many e new-mapping)) (cddr expr)))))
            (else (map (lambda (e) (substitute-many e mapping)) expr)))))

(define (perform-inlining var val body)
  (hashtable-set! *inlining-depth* var (+ (hashtable-ref *inlining-depth* var 0) 1))
  (map (lambda (e) (substitute-proc e var val)) body))

(define (should-inline? var val body-usage)
  (let ((count (car (hashtable-ref body-usage var '(0 . #f)))))
    (or (<= count 1)
        (and (small-procedure? val)
             (< (hashtable-ref *inlining-depth* var 0) 2)))))

(define (try-drop-lambda var val if-expr)
  (let ((test  (cadr  if-expr))
        (then (caddr if-expr))
        (els  (if (null? (cdddr if-expr)) '(unspecified) (cadddr if-expr))))
    (let ((u-test (memq var (analyze-used-vars test)))
          (u-then (memq var (analyze-used-vars then)))
          (u-els  (memq var (analyze-used-vars els))))
      (cond ((and (not u-test) u-then (not u-els))
             (cons #t `(if ,test (let ((,var ,val)) ,then) ,els)))
            ((and (not u-test) (not u-then) u-els)
             (cons #t `(if ,test ,then (let ((,var ,val)) ,els))))
            (else (cons #f #f))))))

;;=============================================================================
;; 4. Core Handlers Helpers
;;=============================================================================

(define (simplify-known-val var expr known-true?)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((eq? (car expr) 'lambda)
     (if (memq var (flatten-params-opt (cadr expr)))
         expr
         `(lambda ,(cadr expr) ,@(map (lambda (e) (simplify-known-val var e known-true?)) (cddr expr)))))
    ((eq? (car expr) 'let)
     (let* ((bindings (cadr expr))
            (vars     (map car bindings))
            (n-bind   (map (lambda (b) (list (car b) (simplify-known-val var (cadr b) known-true?))) bindings)))
       (if (memq var vars)
           `(let ,n-bind ,@(cddr expr))
           `(let ,n-bind ,@(map (lambda (e) (simplify-known-val var e known-true?)) (cddr expr))))))
    ((eq? (car expr) 'letrec*)
     (let* ((bindings (cadr expr))
            (vars     (map car bindings))
            (n-bind   (map (lambda (b) (list (car b) (simplify-known-val var (cadr b) known-true?))) bindings)))
       (if (memq var vars)
           `(letrec* ,n-bind ,@(cddr expr))
           `(letrec* ,n-bind ,@(map (lambda (e) (simplify-known-val var e known-true?)) (cddr expr))))))
    ((eq? (car expr) 'if)
     (let ((test (cadr  expr)) (then (caddr expr)) (els (if (null? (cdddr expr)) '(unspecified) (cadddr expr))))
       (if (eq? test var)
           (if known-true? (simplify-known-val var then #t) (simplify-known-val var els #f))
           `(if ,(simplify-known-val var test known-true?)
                ,(simplify-known-val var then known-true?)
                ,(simplify-known-val var els  known-true?)))))
    ((eq? (car expr) 'begin) `(begin ,@(map (lambda (e) (simplify-known-val var e known-true?)) (cdr expr))))
    (else (map (lambda (e) (simplify-known-val var e known-true?)) expr))))

(define (opt-let-inner bindings body bound-vars)
  (let ((floated '()) (main '()))
    (for-each (lambda (b)
                (let ((var (car b)) (val (cadr b)))
                  (if (and (pair? val) (eq? (car val) 'let))
                      (begin (set! floated (append floated (cadr val)))
                             (set! main (append main (list (list var (if (null? (cddr val)) '() (car (reverse (cddr val)))))))))
                      (set! main (append main (list b))))))
              bindings)
    (if (not (null? floated))
        (optimize-inner `(let ,floated (let ,main ,@body)) bound-vars)
        (let-values (((mutated usage) (analyze-body-combined (make-seq body) '())))
          (let ((new-bind '()) (subst '()))
            (for-each (lambda (b)
                        (let* ((var (car b)) (val (cadr b)) (count (car (hashtable-ref usage var '(0 . #f)))))
                          (cond ((and (not (memq var mutated)) (safe-to-inline-val? var val usage bound-vars mutated)
                                      (or (not (pair? val)) (eq? (car val) 'quote) (symbol? val)
                                          (and (not (has-effects? val)) (<= count 1))))
                                 (set! subst (cons (cons var val) subst))
                                 (if (has-effects? val) (set! new-bind (cons b new-bind))))
                                ((and (not (memq var mutated)) (pair? val) (eq? (car val) 'lambda) (should-inline? var val usage))
                                 (set! body (perform-inlining var val body))
                                 (if (has-effects? val) (set! new-bind (cons b new-bind))))
                                ((and (not (memq var mutated)) (pair? val) (eq? (car val) 'lambda) (and (pair? (car body)) (eq? (car (car body)) 'if)))
                                 (let ((res (try-drop-lambda var val (car body))))
                                   (if (car res) (set! body (list (cdr res))) (set! new-bind (cons b new-bind)))))
                                ((or (> count 0) (memq var mutated) (has-effects? val))
                                 (set! new-bind (cons b new-bind))))))
                      main)
            (unless (null? subst) (set! body (map (lambda (e) (substitute-many e subst)) body)))
            (set! new-bind (reverse new-bind))
            (if (null? new-bind) (if (null? (cdr body)) (car body) `(begin ,@body)) `(let ,new-bind ,@body)))))))

;;=============================================================================
;; 5. Core Handlers
;;=============================================================================

(define (optimize-inner expr bound-vars)
  (cond ((symbol? expr) (if (and (hashtable-contains? global-env expr) (not (memq expr bound-vars))) (hashtable-ref global-env expr #f) expr))
        ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'if) (opt-if expr bound-vars))
        ((eq? (car expr) 'begin) (opt-begin expr bound-vars))
        ((eq? (car expr) 'lambda) (opt-lambda expr bound-vars))
        ((eq? (car expr) 'let) (opt-let expr bound-vars))
        ((eq? (car expr) 'letrec*) (opt-letrec* expr bound-vars))
        ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(optimize-inner (caddr expr) bound-vars)))
        ((eq? (car expr) 'define)
         (let ((var (cadr expr)) (val (optimize-inner (caddr expr) bound-vars)))
           (if (or (not (pair? val)) (eq? (car val) 'quote)) (hashtable-set! global-env var val))
           `(define ,var ,val)))
        (else (opt-app expr bound-vars))))

(define (opt-if expr bound-vars)
  (let ((test (optimize-inner (cadr expr) bound-vars))
        (then (optimize-inner (caddr expr) bound-vars))
        (els  (if (null? (cdddr expr)) '(unspecified) (optimize-inner (cadddr expr) bound-vars))))
    (cond ((and (pair? test) (eq? (car test) 'if)
                ;; Only lift when then+else are cheap to duplicate — otherwise blowup is exponential
                ;; for deeply nested (if (if ...) T E) patterns (e.g. generated pattern-matching code).
                (<= (+ (compute-score then *cp0-effort-limit*)
                       (compute-score els  *cp0-effort-limit*))
                    *cp0-score-limit*))
           (let ((a (cadr test)) (b (caddr test)) (c (cadddr test)))
             (optimize-inner `(if ,a (if ,b ,then ,els) (if ,c ,then ,els)) bound-vars)))
          ((and (pair? test) (eq? (car test) 'quote)) (if (cadr test) then els))
          ((boolean? test) (if test then els))
          (else (if (and (symbol? test) (not (memq test (analyze-mutated-vars-optimizer (make-seq (list then els))))))
                    `(if ,test ,(simplify-known-val test then #t) ,(simplify-known-val test els #f))
                    `(if ,test ,then ,els))))))

(define (opt-begin expr bound-vars)
  (let ((flattened (let loop ((in (cdr expr)) (acc '()))
                     (if (null? in) (reverse acc)
                         (let ((x (optimize-inner (car in) bound-vars)))
                           (if (and (pair? x) (eq? (car x) 'begin))
                               (loop (cdr in) (fold (lambda (i a) (cons i a)) acc (cdr x)))
                               (loop (cdr in) (cons x acc))))))))
    (if (null? flattened) '(unspecified)
        (let* ((last (car (reverse flattened))) (filtered (filter has-effects? (reverse (cdr (reverse flattened))))))
          (if (null? filtered) last `(begin ,@filtered ,last))))))

(define (opt-lambda expr bound-vars)
  (let* ((params (cadr expr)) (new-bound (append (if (list? params) params (list params)) bound-vars))
         (body (map (lambda (e) (optimize-inner e new-bound)) (cddr expr))))
    `(lambda ,params ,@body)))

(define (opt-let expr bound-vars)
  (let* ((bindings (map (lambda (b) (list (car b) (optimize-inner (cadr b) bound-vars))) (cadr expr)))
         (vars (map car bindings)) (new-bound (append vars bound-vars))
         (body (map (lambda (e) (optimize-inner e new-bound)) (cddr expr)))
         (flat-body (if (and (null? (cdr body)) (pair? (car body)) (eq? (car (car body)) 'begin)) (cdr (car body)) body)))
    (opt-let-inner bindings flat-body bound-vars)))

(define (opt-letrec* expr bound-vars)
  (let* ((bindings (cadr expr))
         (vars (map car bindings))
         (new-bound (append vars bound-vars))
         (opt-bindings (map (lambda (b) (list (car b) (optimize-inner (cadr b) new-bound))) bindings))
         (body (map (lambda (e) (optimize-inner e new-bound)) (cddr expr)))
         (flat-body (if (and (null? (cdr body)) (pair? (car body)) (eq? (car (car body)) 'begin)) (cdr (car body)) body)))
    ;; Try to convert to let if no binding references a letrec*-bound var in its init
    (let ((can-be-let? (every (lambda (b)
                               (let ((fvs (analyze-free-vars-optimizer (cadr b) '())))
                                 (not (any (lambda (v) (memq v vars)) fvs))))
                             opt-bindings)))
      (if can-be-let?
          (opt-let-inner opt-bindings flat-body bound-vars)
          `(letrec* ,opt-bindings ,@flat-body)))))

(define (opt-app expr bound-vars)
  (let ((proc (optimize-inner (car expr) bound-vars)) (args (map (lambda (e) (optimize-inner e bound-vars)) (cdr expr))))
    (if (and (pair? proc) (eq? (car proc) 'lambda))
        (let ((params (cadr proc)) (body (cddr proc)))
          (if (and (list? params) (= (length params) (length args))) (optimize-inner `(let ,(map list params args) ,@body) bound-vars)
              (if (and (list? params) (< (length params) (length args)))
                  (let ((extra (list-tail args (length params))))
                    (optimize-inner `(begin ,@extra (let ,(map list params (list-head args (length params))) ,@body)) bound-vars))
                  (cons proc args))))
        (cons proc args))))

(define (optimize-once expr) (optimize-inner expr '()))
#;(define (optimize expr) expr)
(define (optimize expr)
  (hashtable-clear! global-env) (hashtable-clear! *inlining-depth*)
  (let loop ((current expr) (prev '()) (iters 0))
    (if (or (equal? current prev) (>= iters 10)) current
        (loop (optimize-inner current '()) current (+ iters 1)))))
