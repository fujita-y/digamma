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
(define *licm-count* 0)

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

(define (is-licm-var? sym)
  (let ((s (symbol->string sym)))
    (and (> (string-length s) 5)
         (string=? (substring s 0 5) "licm."))))

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
            (else
             (+ 1 (apply + (map (lambda (e) (compute-score e (- effort 1))) expr)))))))

(define (small-procedure? val)
  (<= (compute-score val *cp0-effort-limit*) *cp0-score-limit*))

(define (analyze-used-vars expr)
  (cond ((symbol? expr) (list expr))
        ((not (pair? expr)) '())
        ((eq? (car expr) 'quote) '())
        ((eq? (car expr) 'lambda)
         (let ((p (cadr expr)))
           (set-minus (analyze-used-vars (make-seq (cddr expr))) (if (list? p) p (list p)))))
        ((eq? (car expr) 'let)
         (let ((bindings (cadr expr))
               (body (cddr expr)))
           (set-union (apply set-union (map (lambda (b) (analyze-used-vars (cadr b))) bindings))
                      (set-minus (analyze-used-vars (make-seq body)) (map car bindings)))))
        (else
         (apply set-union (map analyze-used-vars expr)))))

(define (analyze-free-vars-optimizer expr bound-vars)
  (cond ((symbol? expr)
         (if (memq expr bound-vars) '() (list expr)))

        ((not (pair? expr)) '())

        ((eq? (car expr) 'quote) '())

        ((eq? (car expr) 'lambda)
         (let ((p (cadr expr)))
           (analyze-free-vars-optimizer (make-seq (cddr expr))
                                          (set-union (if (list? p) p (list p)) bound-vars))))

        ((eq? (car expr) 'let)
         (let ((bindings (cadr expr))
               (body (cddr expr)))
           (set-union (apply set-union (map (lambda (v) (analyze-free-vars-optimizer v bound-vars))
                                                (map cadr bindings)))
                      (analyze-free-vars-optimizer (make-seq body)
                                                   (set-union (map car bindings) bound-vars)))))

        ((eq? (car expr) 'if)
         (set-union (analyze-free-vars-optimizer (cadr expr) bound-vars)
                    (analyze-free-vars-optimizer (caddr expr) bound-vars)
                    (if (null? (cdddr expr)) '() (analyze-free-vars-optimizer (cadddr expr) bound-vars))))

        ((eq? (car expr) 'begin)
         (apply set-union (map (lambda (e) (analyze-free-vars-optimizer e bound-vars)) (cdr expr))))

        ((eq? (car expr) 'set!)
         (set-union (if (memq (cadr expr) bound-vars) '() (list (cadr expr)))
                    (analyze-free-vars-optimizer (caddr expr) bound-vars)))

        ((eq? (car expr) 'define)
         (analyze-free-vars-optimizer (caddr expr) bound-vars))

        (else
         (apply set-union (map (lambda (e) (analyze-free-vars-optimizer e bound-vars)) expr)))))

(define (analyze-mutated-vars-optimizer expr)
  (cond ((not (pair? expr)) '())
        ((eq? (car expr) 'quote) '())
        ((eq? (car expr) 'set!)
         (cons (cadr expr) (analyze-mutated-vars-optimizer (caddr expr))))
        ((eq? (car expr) 'lambda)
         (let ((p (cadr expr)))
           (set-minus (apply set-union (map analyze-mutated-vars-optimizer (cddr expr)))
                      (if (list? p) p (list p)))))
        (else
         (apply set-union (map analyze-mutated-vars-optimizer expr)))))

(define (analyze-free-var-counts expr)
  (let ((counts (make-eq-hashtable)))
    (let walk ((e expr) (shadowed '()))
      (cond ((symbol? e)
             (unless (memq e shadowed)
               (hashtable-set! counts e (+ (hashtable-ref counts e 0) 1))))

            ((not (pair? e)) #f)

            ((eq? (car e) 'quote) #f)

            ((eq? (car e) 'lambda)
             (let ((new-shadowed (let ((p (cadr e))) (if (list? p) (append p shadowed) (cons p shadowed)))))
               (for-each (lambda (be) (walk be new-shadowed)) (cddr e))))

            ((eq? (car e) 'let)
             (let* ((bindings (cadr e))
                    (vars (map car bindings))
                    (inits (map cadr bindings)))
               (for-each (lambda (init) (walk init shadowed)) inits)
               (for-each (lambda (be) (walk be (append vars shadowed))) (cddr e))))

            ((eq? (car e) 'set!)
             (walk (caddr e) shadowed))

            (else
             (for-each (lambda (x) (walk x shadowed)) e))))
    counts))

(define (has-effects? expr)
  (cond ((symbol? expr) #f)
        ((not (pair? expr)) #f)
        ((eq? (car expr) 'quote) #f)
        ((eq? (car expr) 'lambda) #f)
        ((eq? (car expr) 'if)
         (or (has-effects? (cadr expr))
             (has-effects? (caddr expr))
             (and (not (null? (cdddr expr))) (has-effects? (cadddr expr)))))
        ((eq? (car expr) 'begin)
         (any has-effects? (cdr expr)))
        ((eq? (car expr) 'set!) #t)
        ((eq? (car expr) 'define) #t)
        ((eq? (car expr) 'let)
         (or (any (lambda (b) (has-effects? (cadr b))) (cadr expr))
             (any has-effects? (cddr expr))))
        (else
         (not (and (memq (car expr) pure-primitives)
                   (not (any has-effects? (cdr expr))))))))

;;=============================================================================
;; 3. Transformation Helpers
;;=============================================================================

(define (substitute expr var val)
  (cond ((eq? expr var) val)
        ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'lambda)
         (if (member var (let ((p (cadr expr))) (unique (if (list? p) p (list p)))))
             expr
             `(lambda ,(cadr expr) ,@(map (lambda (e) (substitute e var val)) (cddr expr)))))
        ((eq? (car expr) 'let)
         (let ((vars (map car (cadr expr))))
           (if (memq var vars)
               `(let ,(map (lambda (b) (list (car b) (substitute (cadr b) var val))) (cadr expr)) ,@(cddr expr))
               `(let ,(map (lambda (b) (list (car b) (substitute (cadr b) var val))) (cadr expr)) ,@(map (lambda (e) (substitute e var val)) (cddr expr))))))
        (else
         (map (lambda (e) (substitute e var val)) expr))))

(define (substitute-proc expr var val)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'lambda)
         (if (memq var (let ((p (cadr expr))) (if (list? p) p (list p))))
             expr
             `(lambda ,(cadr expr) ,@(map (lambda (e) (substitute-proc e var val)) (cddr expr)))))
        ((eq? (car expr) var)
         (cons val (map (lambda (e) (substitute-proc e var val)) (cdr expr))))
        (else
         (let ((new-expr (map (lambda (e) (substitute-proc e var val)) expr)))
           (if (and (pair? (car new-expr)) (eq? (car (car new-expr)) 'lambda))
               (optimize-once new-expr)
               new-expr)))))

(define (perform-inlining var val body)
  (hashtable-set! *inlining-depth* var (+ (hashtable-ref *inlining-depth* var 0) 1))
  (map (lambda (e) (substitute-proc e var val)) body))

(define (should-inline? var val body)
  (let ((count (hashtable-ref (analyze-free-var-counts body) var 0)))
    (or (<= count 1)
        (and (small-procedure? val)
             (< (hashtable-ref *inlining-depth* var 0) 2)))))

(define (try-drop-lambda var val if-expr)
  (let* ((test-clause  (cadr if-expr))
         (then-clause (caddr if-expr))
         (else-clause (if (null? (cdddr if-expr)) '(unspecified) (cadddr if-expr)))
         (used-in-test (memq var (analyze-used-vars test-clause)))
         (used-in-then (memq var (analyze-used-vars then-clause)))
         (used-in-else (memq var (analyze-used-vars else-clause))))
    (cond ((and (not used-in-test) used-in-then (not used-in-else))
           (cons #t `(if ,test-clause (let ((,var ,val)) ,then-clause) ,else-clause)))
          ((and (not used-in-test) (not used-in-then) used-in-else)
           (cons #t `(if ,test-clause ,then-clause (let ((,var ,val)) ,else-clause))))
          (else
           (cons #f #f)))))

;;=============================================================================
;; 4. Core Handlers Helpers
;;=============================================================================

(define (opt-licm expr bound-vars)
  (let* ((bindings (cadr expr))
         (loop-name (caar bindings))
         (init-val (cadar bindings))
         (body-seq (cddr expr))
         (set-expr (car body-seq))
         (loop-lambda (caddr set-expr))
         (params (cadr loop-lambda))
         (lambda-body (cddr loop-lambda)))
    
    (let* ((params-list (flatten-params-opt params))
           (mutated (analyze-mutated-vars-optimizer (make-seq lambda-body)))
           (forbidden (append params-list mutated))
           (hoisted-map (make-equal-hashtable))
           (hoisted-list '()))

      (define (transform e forbidden)
        (cond
          ((not (pair? e)) e)
          ((eq? (car e) 'quote) e)
          ((and (memq (car e) pure-primitives)
                (not (has-effects? e))
                (let ((fvars (analyze-free-vars-optimizer e '())))
                  (not (any (lambda (v) (memq v forbidden)) fvars))))
            (if (hashtable-contains? hoisted-map e)
                (hashtable-ref hoisted-map e #f)
                (let ((tmp (generate-temporary-symbol "licm.")))
                  (hashtable-set! hoisted-map e tmp)
                  (set! hoisted-list (cons (list tmp e) hoisted-list))
                  tmp)))
          ((eq? (car e) 'lambda)
           (let ((new-forbidden (append (flatten-params-opt (cadr e)) forbidden)))
             `(lambda ,(cadr e) ,@(map (lambda (x) (transform x new-forbidden)) (cddr e)))))
          ((eq? (car e) 'let)
           (let* ((bs (cadr e))
                  (vars (map car bs))
                  (new-forbidden (append vars forbidden)))
             `(let ,(map (lambda (b) (list (car b) (transform (cadr b) forbidden))) bs)
                ,@(map (lambda (x) (transform x new-forbidden)) (cddr e)))))
          ((eq? (car e) 'if)
           `(if ,(transform (cadr e) forbidden)
                ,(transform (caddr e) forbidden)
                ,(if (null? (cadddr e)) '(unspecified) (transform (cadddr e) forbidden))))
          ((eq? (car e) 'set!)
           `(set! ,(cadr e) ,(transform (caddr e) forbidden)))
          ((eq? (car e) 'begin)
           `(begin ,@(map (lambda (x) (transform x forbidden)) (cdr e))))
          ((eq? (car e) 'define)
           `(define ,(cadr e) ,(transform (caddr e) forbidden)))
          (else
           (map (lambda (x) (transform x forbidden)) e))))

      (let ((new-lambda-body (map (lambda (e) (transform e forbidden)) lambda-body)))
        (if (null? hoisted-list)
            #f ;; No changes
            `(let ,(reverse hoisted-list)
               (let ((,loop-name ,init-val))
                 (set! ,loop-name (lambda ,params ,@new-lambda-body))
                 ,@(cdr body-seq))))))))

(define (opt-let-inner bindings body bound-vars)
    ;; Let-floating
    (let ((floated-bindings '())
          (main-bindings '()))
      (for-each (lambda (b)
                  (let ((var (car b)) (val (cadr b)))
                    (if (and (pair? val) (eq? (car val) 'let))
                        (begin
                          (set! floated-bindings (append floated-bindings (cadr val)))
                          (set! main-bindings (append main-bindings
                                                       (list (list var (if (null? (cddr val))
                                                                           '()
                                                                           (car (reverse (cddr val)))))))))
                        (set! main-bindings (append main-bindings (list b))))))
                bindings)

      (if (not (null? floated-bindings))
          (optimize-inner `(let ,floated-bindings (let ,main-bindings ,@body)) bound-vars)

          ;; Substitution / Inlining Pipeline
          (let ((used (analyze-used-vars (make-seq body)))
                (mutated-vars (analyze-mutated-vars-optimizer (make-seq body)))
                (new-bindings '())
                (var-counts #f)
                (body-changed? #t))

            (define (get-use-count var current-body)
              (when body-changed?
                (set! var-counts (analyze-free-var-counts current-body))
                (set! body-changed? #f))
              (hashtable-ref var-counts var 0))

              (define (process-binding! b)
                (let ((var (car b)) (val (cadr b)))
                  (cond
                    ((and (not (memq var mutated-vars))
                          (not (is-licm-var? var))
                          (or (not (pair? val)) (and (pair? val) (eq? (car val) 'quote)) (symbol? val)
                            (and (not (has-effects? val)) (<= (get-use-count var (make-seq body)) 1))))
                   (set! body (map (lambda (e) (substitute e var val)) body))
                   (set! body-changed? #t)
                   (if (has-effects? val) (set! new-bindings (cons b new-bindings))))

                    ((and (not (memq var mutated-vars))
                          (pair? val)
                          (eq? (car val) 'lambda)
                          (not (is-licm-var? var))
                          (should-inline? var val (make-seq body)))
                   (set! body (perform-inlining var val body))
                   (set! body-changed? #t)
                   (if (has-effects? val) (set! new-bindings (cons b new-bindings))))

                  ((and (not (memq var mutated-vars))
                        (pair? val)
                        (eq? (car val) 'lambda)
                        (and (pair? (car body)) (eq? (car (car body)) 'if)))
                   (let ((res (try-drop-lambda var val (car body))))
                     (if (car res)
                         (begin (set! body (list (cdr res))) (set! body-changed? #t))
                         (set! new-bindings (cons b new-bindings)))))

                  ((or (memq var used) (memq var mutated-vars) (has-effects? val))
                   (set! new-bindings (cons b new-bindings))))))

            (for-each process-binding! main-bindings)
            (set! new-bindings (reverse new-bindings))

            (if (null? new-bindings)
                (if (null? (cdr body)) (car body) `(begin ,@body))
                `(let ,new-bindings ,@body))))))

;;=============================================================================
;; 5. Core Handlers
;;=============================================================================

(define (optimize-inner expr bound-vars)
  (cond
    ((symbol? expr)
     (if (and (hashtable-contains? global-env expr)
              (not (memq expr bound-vars)))
         (hashtable-ref global-env expr #f)
         expr))

    ((not (pair? expr)) expr)

    ((eq? (car expr) 'quote) expr)

    ((eq? (car expr) 'if) (opt-if expr bound-vars))

    ((eq? (car expr) 'begin) (opt-begin expr bound-vars))

    ((eq? (car expr) 'lambda) (opt-lambda expr bound-vars))

    ((eq? (car expr) 'let) (opt-let expr bound-vars))

    ((eq? (car expr) 'set!)
     `(set! ,(cadr expr) ,(optimize-inner (caddr expr) bound-vars)))

    ((eq? (car expr) 'define)
     (let ((var (cadr expr))
           (val (optimize-inner (caddr expr) bound-vars)))
       (if (or (not (pair? val)) (and (pair? val) (eq? (car val) 'quote)))
           (hashtable-set! global-env var val))
       `(define ,var ,val)))

    (else (opt-app expr bound-vars))))

(define (opt-if expr bound-vars)
  (let ((test (optimize-inner (cadr expr) bound-vars))
        (then (optimize-inner (caddr expr) bound-vars))
        (els (if (null? (cdddr expr)) '(unspecified) (optimize-inner (cadddr expr) bound-vars))))
    (cond
      ;; If-lifting: (if (if a b c) d e) -> (if a (if b d e) (if c d e))
      ((and (pair? test) (eq? (car test) 'if))
       (let ((a (cadr test)) (b (caddr test)) (c (cadddr test)))
         (optimize-inner `(if ,a (if ,b ,then ,els) (if ,c ,then ,els)) bound-vars)))

      ((and (pair? test) (eq? (car test) 'quote))
       (if (cadr test) then els))

      ((boolean? test)
       (if test then els))

      (else
       `(if ,test ,then ,els)))))

(define (opt-begin expr bound-vars)
  (let* ((exprs (map (lambda (e) (optimize-inner e bound-vars)) (cdr expr)))
         (flattened (apply append (map (lambda (x) (if (and (pair? x) (eq? (car x) 'begin)) (cdr x) (list x))) exprs)))
         (filtered (filter (lambda (x) (has-effects? x)) (take flattened (max 0 (- (length flattened) 1))))))
    (let ((last-val (if (null? flattened) '(unspecified) (car (reverse flattened)))))
      (if (null? filtered)
          last-val
          `(begin ,@filtered ,last-val)))))

(define (opt-lambda expr bound-vars)
  (let* ((params (cadr expr))
         (new-bound (if (list? params) (append params bound-vars) (cons params bound-vars)))
         (body-exprs (map (lambda (e) (optimize-inner e new-bound)) (cddr expr)))
         (body (make-seq body-exprs))
         (used (analyze-used-vars body))
         (mutated (analyze-mutated-vars-optimizer body)))
    (let loop ((ps (if (list? params) params '()))
               (new-params '()))
      (cond
        ((null? ps)
         `(lambda ,(if (list? params) (reverse new-params) params) ,@body-exprs))
        (else
         (loop (cdr ps) (cons (car ps) new-params)))))))

(define (opt-let expr bound-vars)
  (let* ((bindings (map (lambda (b) (list (car b) (optimize-inner (cadr b) bound-vars))) (cadr expr)))
         (vars (map car bindings))
         (new-bound (append vars bound-vars))
         (body-exprs (map (lambda (e) (optimize-inner e new-bound)) (cddr expr)))
         (body (if (and (null? (cdr body-exprs))
                        (pair? (car body-exprs))
                        (eq? (car (car body-exprs)) 'begin))
                   (cdr (car body-exprs))
                   body-exprs)))

    (let ((candidate `(let ,bindings ,@body)))
      (if (match-rec-pattern candidate)
          (let ((licm-res (opt-licm candidate bound-vars)))
             (if licm-res licm-res (opt-let-inner bindings body bound-vars)))
          (opt-let-inner bindings body bound-vars)))))

(define (opt-app expr bound-vars)
  (let ((proc (optimize-inner (car expr) bound-vars))
        (args (map (lambda (e) (optimize-inner e bound-vars)) (cdr expr))))
    (cond
      ;; Beta-reduction
      ((and (pair? proc) (eq? (car proc) 'lambda))
       (let ((params (cadr proc)) (body (cddr proc)))
         (if (and (list? params) (= (length params) (length args)))
             (optimize-inner `(let ,(map list params args) ,@body) bound-vars)
             (if (and (list? params) (< (length params) (length args)))
                 (let ((extra-args (drop args (length params))))
                   (optimize-inner `(begin ,@extra-args (let ,(map list params (take args (length params))) ,@body)) bound-vars))
                 (cons proc args)))))
      (else (cons proc args)))))

;;=============================================================================
;; 6. Dispatcher & Entry Points
;;=============================================================================

;; Perform a single optimization pass.
(define (optimize-once expr)
  (optimize-inner expr '()))

;; Perform full optimization until fixed-point or iteration limit.
(define (optimize expr)
  (hashtable-clear! global-env)
  (hashtable-clear! *inlining-depth*)
  (let loop ((current expr) (prev '()) (iters 0))
    (if (or (equal? current prev) (>= iters 10))
        current
        (loop (optimize-inner current '()) current (+ iters 1)))))
