;;; types.scm
;;; Type Speculation for Scheme Programs
;;; Annotates variable references with inferred types based on R7RS/R6RS primitives

;; --- Type Environment ---
;; Environment maps variable names to types.
;; Using SRFI-69 or Gauche hash tables for efficiency.
(use srfi-69)
(use srfi-13)

(define (ty:make-env)
  (make-hash-table 'eq?))

(define (ty:env-get env var default)
  (hash-table-get env var default))

(define (ty:env-set! env var type)
  (hash-table-put! env var type))

(define (ty:env-copy env)
  (hash-table-copy env))

;; Merge two environments (for control flow joins)
;; If a variable has different types in the two envs, use 'any
(define (ty:env-merge env1 env2)
  (let ((result (ty:env-copy env1)))
    ;; Add/merge entries from env2
    (hash-table-for-each env2
      (lambda (k v2)
        (let ((v1 (ty:env-get result k #f))) ; #f if not in env1
          (cond
            ((not v1) ; k is only in env2
             (ty:env-set! result k v2))
            ((not (eq? v1 v2)) ; k is in both, but types differ
             (ty:env-set! result k 'any))
            (else #f))))) ; k is in both, types are same, no change needed
    result))

;; --- Primitive Type Signatures ---
;; Returns the result type of a primitive operation
;; Format: 'primitive-name -> 'result-type
;; Some primitives also have argument type requirements

(define ty:primitive-result-types
  (let ((ht (make-hash-table 'eq?)))
    ;; Arithmetic - all produce numbers
    (for-each (lambda (p) (hash-table-put! ht p 'number))
              '(+ - * / quotient remainder modulo
                abs floor ceiling truncate round
                exp log sin cos tan asin acos atan
                sqrt expt gcd lcm
                numerator denominator
                exact inexact exact->inexact inexact->exact
                number->string string->number))
    
    ;; Comparisons - all produce booleans  
    (for-each (lambda (p) (hash-table-put! ht p 'boolean))
              '(= < > <= >= zero? positive? negative?
                odd? even? exact? inexact?
                eq? eqv? equal?
                boolean? number? char? string? symbol? pair? null?
                list? vector? bytevector? procedure? port?
                input-port? output-port? textual-port? binary-port?
                eof-object?))
    
    ;; List/Pair operations
    (hash-table-put! ht 'cons 'pair)
    (hash-table-put! ht 'list 'any)  ;; Can be null (if no args) or pair
    (hash-table-put! ht 'car 'any)
    (hash-table-put! ht 'cdr 'any)
    (hash-table-put! ht 'caar 'any)
    (hash-table-put! ht 'cadr 'any)
    (hash-table-put! ht 'cdar 'any)
    (hash-table-put! ht 'cddr 'any)
    (hash-table-put! ht 'caaar 'any)
    (hash-table-put! ht 'caadr 'any)
    (hash-table-put! ht 'cadar 'any)
    (hash-table-put! ht 'caddr 'any)
    (hash-table-put! ht 'cdaar 'any)
    (hash-table-put! ht 'cdadr 'any)
    (hash-table-put! ht 'cddar 'any)
    (hash-table-put! ht 'cdddr 'any)
    (hash-table-put! ht 'reverse 'pair)
    (hash-table-put! ht 'append 'pair)
    (hash-table-put! ht 'length 'number)
    (hash-table-put! ht 'list-ref 'any)
    (hash-table-put! ht 'list-tail 'pair)
    (hash-table-put! ht 'memq 'any)  ;; Returns pair or #f
    (hash-table-put! ht 'memv 'any)
    (hash-table-put! ht 'member 'any)
    (hash-table-put! ht 'assq 'any)
    (hash-table-put! ht 'assv 'any)
    (hash-table-put! ht 'assoc 'any)
    
    ;; String operations
    (hash-table-put! ht 'make-string 'string)
    (hash-table-put! ht 'string 'string)
    (hash-table-put! ht 'string-append 'string)
    (hash-table-put! ht 'substring 'string)
    (hash-table-put! ht 'string-copy 'string)
    (hash-table-put! ht 'string-length 'number)
    (hash-table-put! ht 'string-ref 'char)
    (hash-table-put! ht 'string->list 'pair)
    (hash-table-put! ht 'list->string 'string)
    (hash-table-put! ht 'string->symbol 'symbol)
    (hash-table-put! ht 'symbol->string 'string)
    
    ;; Vector operations
    (hash-table-put! ht 'make-vector 'vector)
    (hash-table-put! ht 'vector 'vector)
    (hash-table-put! ht 'vector-length 'number)
    (hash-table-put! ht 'vector-ref 'any)
    (hash-table-put! ht 'vector->list 'pair)
    (hash-table-put! ht 'list->vector 'vector)
    
    ;; Bytevector operations
    (hash-table-put! ht 'make-bytevector 'bytevector)
    (hash-table-put! ht 'bytevector 'bytevector)
    (hash-table-put! ht 'bytevector-length 'number)
    (hash-table-put! ht 'bytevector-u8-ref 'number)
    
    ;; Character operations
    (hash-table-put! ht 'char->integer 'number)
    (hash-table-put! ht 'integer->char 'char)
    (hash-table-put! ht 'char-upcase 'char)
    (hash-table-put! ht 'char-downcase 'char)
    
    ;; Not operation
    (hash-table-put! ht 'not 'boolean)
    
    ht))

;; Helper to check if a primitive is a list accessor (car, cdr, caar, etc.)
(define (ty:list-accessor? op)
  (let ((s (symbol->string op)))
    (and (>= (string-length s) 3)
         (char=? (string-ref s 0) #\c)
         (char=? (string-ref s (- (string-length s) 1)) #\r)
         (let loop ((i 1))
           (if (= i (- (string-length s) 1))
               #t
               (let ((c (string-ref s i)))
                 (if (or (char=? c #\a) (char=? c #\d))
                     (loop (+ i 1))
                     #f)))))))

;; Helper to get the confirmed path of a pair type (pair = "", pair:aad = "aad")
(define (ty:type->path t)
  (if (eq? t 'any) #f
      (let ((ts (symbol->string t)))
        (cond
          ((string=? ts "pair") "")
          ;; Optimize prefix check with srfi-13
          ((and (> (string-length ts) 5) (string-prefix? "pair:" ts))
           (substring ts 5 (string-length ts)))
          (else #f)))))

;; Helper to infer the result type of a list accessor based on argument type
(define (ty:infer-accessor-result op arg-type)
  (if (not (ty:list-accessor? op)) 'any
      (let ((path (ty:type->path arg-type)))
        (if (not path) 'any
            (let ((s (symbol->string op)))
              (if (= (string-length s) 3) ;; car or cdr
                  (let ((accessor-char (string-ref s 1)))
                    (if (and (> (string-length path) 0) (char=? (string-ref path 0) accessor-char))
                        (let ((rem (substring path 1 (string-length path))))
                          (if (string=? rem "") 'pair (string->symbol (string-append "pair:" rem))))
                        'any))
                  'any)))))) ;; For now, only infer one-level car/cdr from a path

;; Helper to refine a type to a deeper path
(define (ty:refine-pair-path path current-type)
  (let ((curr-path (ty:type->path current-type)))
    (if (not curr-path)
        (if (string=? path "") 'pair (string->symbol (string-append "pair:" path)))
        (if (> (string-length path) (string-length curr-path))
            (string->symbol (string-append "pair:" path))
            current-type))))

;; Primitives that imply specific types for their arguments
;; op -> (list-of-types-or-any)
;; If an argument is a variable, we can infer its type from the usage
(define ty:primitive-arg-requirements
  (let ((ht (make-hash-table 'eq?)))
    ;; List operations expecting pair (or list)
    (for-each (lambda (p) (hash-table-put! ht p '(pair)))
              '(car cdr caar cadr cdar cddr 
                caaar caadr cadar caddr cdaar cdadr cddar cdddr
                set-car! set-cdr!))
    
    ;; Vector operations
    (hash-table-put! ht 'vector-length '(vector))
    (hash-table-put! ht 'vector-ref '(vector number))
    (hash-table-put! ht 'vector-set! '(vector number any))
    
    ;; String operations
    (hash-table-put! ht 'string-length '(string))
    (hash-table-put! ht 'string-ref '(string number))
    (hash-table-put! ht 'string-set! '(string number char))
    
    ;; Arithmetic (assume number for all args for simplicity on common ops)
    (for-each (lambda (p) (hash-table-put! ht p '(number number)))
              '(+ - * / < > <= >= =))
    
    ht))

;; Get the result type of applying a primitive
(define (ty:primitive-result-type op)
  (hash-table-get ty:primitive-result-types op 'any))

;; --- Type Annotation ---
;; Annotates a variable reference with its inferred type.
;; Only annotates symbols (variables) with non-'any' types.
;; All pair variants (pair, pair:aaa, etc.) are annotated as 'type:pair'
;; The annotation is: (begin 'type:TYPENAME variable)
(define (ty:annotate expr type)
  (if (and (symbol? expr) 
           (not (eq? type 'any)))
      (let* ((ts (symbol->string type))
             (actual-type (if (or (string=? ts "pair")
                                  ;; Optimize prefix check with srfi-13
                                  (and (> (string-length ts) 5) (string-prefix? "pair:" ts)))
                              'pair
                              type)))
        `(begin ',(string->symbol (string-append "type:" (symbol->string actual-type))) ,expr))
      expr))

;; --- Type Inference ---
;; Infers the type that an expression produces.
;; Returns a type symbol ('number, 'boolean, 'pair, 'string, etc.) or 'any.
;; Uses the environment to look up variable types and analyzes expression structure
;; to determine result types. Handles:
;; - Literals (numbers, booleans, chars, strings, null)
;; - Variables (lookup in environment)
;; - Quote forms
;; - If expressions (unifies then/else branches)
;; - Lambda (always 'procedure)
;; - Begin (type of last expression)
;; - Let (evaluates body in extended environment)
;; - Applications (uses primitive result types)

;; Helper for inferring type of primitive applications
(define (ty:infer-apply-type op args env)
  (cond
    ;; list: no args → null, with args → pair
    ((eq? op 'list)
     (if (null? args) 'null 'pair))
    
    ;; string->list: empty string → null, non-empty → pair
    ((eq? op 'string->list)
     (if (and (= (length args) 1)
              (string? (car args))
              (zero? (string-length (car args))))
         'null
         (if (and (= (length args) 1) (string? (car args)))
             'pair
             'any)))  ;; Can't determine statically
    
    ;; vector->list: empty vector → null, non-empty → pair
    ((eq? op 'vector->list)
     (if (and (= (length args) 1)
              (vector? (car args))
              (zero? (vector-length (car args))))
         'null
         (if (and (= (length args) 1) (vector? (car args)))
             'pair
             'any)))  ;; Can't determine statically
    
    ;; append: all empty lists → null, otherwise could be pair or null
    ((eq? op 'append)
     (if (null? args)
         'null
         ;; If all args are null literals or quoted null, result is null
         (let ((all-null? 
                 (let loop ((as args))
                   (cond
                     ((null? as) #t)
                     ;; Check for literal null or (quote ())
                     ((or (null? (car as))
                          (and (pair? (car as))
                               (eq? (caar as) 'quote)
                               (null? (cadar as))))
                      (loop (cdr as)))
                     (else #f)))))
           (if all-null? 'null 'any))))
    
    ;; reverse: null → null, otherwise could be pair or null  
    ((eq? op 'reverse)
     (if (and (= (length args) 1)
              (or (null? (car args))
                  (and (pair? (car args))
                       (eq? (caar args) 'quote)
                       (null? (cadar args)))))
         'null
         'any))  ;; Can't determine statically
    
    ;; Default: use primitive result type
    (else 
     (let ((res (ty:primitive-result-type op)))
       (if (and (eq? res 'any) (ty:list-accessor? op) (= (length args) 1))
           (let ((arg-type (ty:infer-expr-type (car args) env)))
             (ty:infer-accessor-result op arg-type))
           res)))))

(define (ty:infer-expr-type expr env)
  (cond
    ;; Literals
    ((number? expr) 'number)
    ((boolean? expr) 'boolean)
    ((char? expr) 'char)
    ((string? expr) 'string)
    ((null? expr) 'null)
    
    ;; Variables - look up in environment
    ((symbol? expr)
     (ty:env-get env expr 'any))
    
    ;; Quote
    ((and (pair? expr) (eq? (car expr) 'quote))
     (let ((val (cadr expr)))
       (cond
         ((number? val) 'number)
         ((boolean? val) 'boolean)
         ((char? val) 'char)
         ((string? val) 'string)
         ((symbol? val) 'symbol)
         ((pair? val) 'pair)
         ((null? val) 'null)
         ((vector? val) 'vector)
         ((bytevector? val) 'bytevector)
         (else 'any))))
    
    ;; If
    ((and (pair? expr) (eq? (car expr) 'if))
     (let ((then-type (ty:infer-expr-type (caddr expr) env))
           (else-type (if (null? (cdddr expr))
                         'any
                         (ty:infer-expr-type (cadddr expr) env))))
       (if (eq? then-type else-type)
           then-type
           'any)))
    
    ;; Lambda
    ((and (pair? expr) (eq? (car expr) 'lambda))
     'procedure)
    
    ;; Begin
    ((and (pair? expr) (eq? (car expr) 'begin))
     (if (null? (cdr expr))
         'any
         (ty:infer-expr-type (car (last-pair (cdr expr))) env)))
    
    ;; Let
    ((and (pair? expr) (eq? (car expr) 'let))
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (let ((new-env (ty:env-copy env)))
         ;; Add bindings to environment
         (for-each
           (lambda (binding)
             (let ((var (car binding))
                   (val (cadr binding)))
               (ty:env-set! new-env var (ty:infer-expr-type val env))))
           bindings)
         ;; Infer type of body
         (if (null? body)
             'any
             (ty:infer-expr-type (car (last-pair body)) new-env)))))
    
    ;; Application
    ((pair? expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (if (symbol? op)
           (ty:infer-apply-type op args env)
           'any)))
    
    (else 'any)))

;; --- Speculation Handlers ---

(define (ty:handle-lambda expr env)
  (let ((params (cadr expr))
        (body (cddr expr)))
    (let ((new-env (ty:env-copy env)))
      ;; Add parameters with 'any type initially
      (let loop ((ps params))
        (cond
          ((null? ps) #f)
          ((symbol? ps) (ty:env-set! new-env ps 'any))
          ((pair? ps)
           (ty:env-set! new-env (car ps) 'any)
           (loop (cdr ps)))))
      `(lambda ,params
         ,@(map (lambda (e) (ty:speculate-inner e new-env)) body)))))

(define (ty:handle-define expr env)
  (if (pair? (cadr expr))
      ;; Function definition: (define (f x) body)
      (let ((name (car (cadr expr)))
            (params (cdr (cadr expr)))
            (body (cddr expr)))
        (let ((new-env (ty:env-copy env)))
          ;; Add parameters with 'any type (handle dotted lists)
          (let loop ((ps params))
            (cond
              ((null? ps) #f)
              ((symbol? ps) (ty:env-set! new-env ps 'any))
              ((pair? ps)
               (ty:env-set! new-env (car ps) 'any)
               (loop (cdr ps)))))
          (ty:env-set! env name 'procedure)
          `(define ,(cadr expr)
             ,@(map (lambda (e) (ty:speculate-inner e new-env)) body))))
      ;; Variable definition: (define x val)
      (let ((var (cadr expr))
            (val (caddr expr)))
        (let* ((val-type (ty:infer-expr-type val env)))
          (ty:env-set! env var val-type)
          (let ((val-speculated (ty:speculate-inner val env)))
            `(define ,var ,val-speculated))))))

(define (ty:handle-set! expr env)
  (let ((var (cadr expr))
        (val (caddr expr)))
    (let* ((val-type (ty:infer-expr-type val env)))
      ;; Update environment before speculating value for recursion
      (ty:env-set! env var val-type)
      (let ((val-speculated (ty:speculate-inner val env)))
        `(set! ,var ,val-speculated)))))

(define (ty:handle-if expr env)
  (let ((test (cadr expr))
        (then-expr (caddr expr))
        (else-expr (if (null? (cdddr expr)) #f (cadddr expr))))
    
    (let ((test-speculated (ty:speculate-inner test env)))
      ;; Try to refine types based on type predicates
      (let ((then-env (ty:env-copy env))
            (else-env (ty:env-copy env)))
        
        ;; If test is a type predicate on a variable, refine the type
        (when (and (pair? test) (symbol? (car test)) (= (length test) 2) (symbol? (cadr test)))
          (let ((pred (car test))
                (var (cadr test)))
            (case pred
              ((pair?) (ty:env-set! then-env var 'pair))
              ((null?) (ty:env-set! then-env var 'null))
              ((number?) (ty:env-set! then-env var 'number))
              ((boolean?) (ty:env-set! then-env var 'boolean))
              ((char?) (ty:env-set! then-env var 'char))
              ((string?) (ty:env-set! then-env var 'string))
              ((symbol?) (ty:env-set! then-env var 'symbol))
              ((vector?) (ty:env-set! then-env var 'vector))
              ((bytevector?) (ty:env-set! then-env var 'bytevector))
              ((procedure?) (ty:env-set! then-env var 'procedure)))))
        
        (let ((then-speculated (ty:speculate-inner then-expr then-env))
              (else-speculated (if else-expr 
                                   (ty:speculate-inner else-expr else-env)
                                   #f)))
          (if else-expr
              `(if ,test-speculated ,then-speculated ,else-speculated)
              `(if ,test-speculated ,then-speculated)))))))

(define (ty:handle-begin expr env)
  (let loop ((es (cdr expr)) (res '()))
    (if (null? es)
        `(begin ,@(reverse res))
        (let ((spec (ty:speculate-inner (car es) env)))
          (loop (cdr es) (cons spec res))))))

(define (ty:handle-let expr env)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (let* ((new-env (ty:env-copy env))
           (new-bindings 
             (map (lambda (b)
                    (let ((var (car b))
                          (val (cadr b)))
                      (let ((spec (ty:speculate-inner val env))
                            (type (ty:infer-expr-type val env)))
                        
                        ;; Flow-sensitive argument refinement (applies to outer scope env)
                        (let refine ((e val) (t 'any))
                          (cond
                            ((symbol? e)
                             (unless (eq? t 'any)
                               (let ((curr (ty:env-get new-env e 'any)))
                                 (ty:env-set! new-env e (if (eq? curr 'any) t 
                                                            (let ((p (ty:type->path t)))
                                                              (if p (ty:refine-pair-path p curr) t)))))))
                            ((and (pair? e) (symbol? (car e)))
                             (let ((op (car e)) (args (cdr e)))
                               (let ((reqs (hash-table-get ty:primitive-arg-requirements op #f)))
                                 (when reqs (let loop ((as args) (rs reqs)) (when (and (pair? as) (pair? rs)) (refine (car as) (car rs)) (loop (cdr as) (cdr rs))))))
                               (when (and (ty:list-accessor? op) (= (length args) 1))
                                 (let* ((s (symbol->string op))
                                        (path (string-reverse (substring s 2 (- (string-length s) 1)))))
                                   (refine (car args) (ty:refine-pair-path path (ty:env-get new-env (if (symbol? (car args)) (car args) 'dummy) 'any)))))))))
                        
                        ;; The variable itself should be added to new-env for the body
                        (ty:env-set! new-env var type)
                        (list var spec))))
                  bindings)))
      `(let ,new-bindings
         ,@(map (lambda (e) (ty:speculate-inner e new-env)) body)))))

(define (ty:handle-app expr env)
  (let ((op (car expr))
        (args (cdr expr)))
    (let ((op-speculated (ty:speculate-inner op env))
          (reqs (if (symbol? op) (hash-table-get ty:primitive-arg-requirements op #f) #f)))
      ;; Sequential speculation and refinement
      (let loop ((as args) (rs (or reqs '())) (res '()))
        (if (null? as)
            `(,op-speculated ,@(reverse res))
            (let* ((arg (car as))
                   (spec (ty:speculate-inner arg env)))
              ;; Record usage-based refinement AFTER speculation
              (when (symbol? arg)
                (let ((curr (ty:env-get env arg 'any)))
                  (when (eq? curr 'any)
                    ;; 1. Refine from primitive requirements table
                    (when (and (pair? rs) (not (eq? (car rs) 'any)))
                      (ty:env-set! env arg (car rs)))
                    ;; 2. Refine from list accessor path
                    (when (and (symbol? op) (ty:list-accessor? op) (= (length args) 1))
                      (let* ((s (symbol->string op))
                             (path (string-reverse (substring s 2 (- (string-length s) 1)))))
                        (ty:env-set! env arg (ty:refine-pair-path path curr)))))))
              (loop (cdr as) (if (null? rs) '() (cdr rs)) (cons spec res))))))))

;; --- Main Speculation Walker ---
(define (ty:speculate-inner expr env)
  (cond
    ;; Literals - no annotation needed
    ((not (pair? expr)) 
     (ty:annotate expr (ty:infer-expr-type expr env)))
    
    ;; Quote - no annotation needed
    ((eq? (car expr) 'quote) expr)
    
    ;; Lambda
    ((eq? (car expr) 'lambda) (ty:handle-lambda expr env))
    
    ;; Define
    ((eq? (car expr) 'define) (ty:handle-define expr env))
    
    ;; Set!
    ((eq? (car expr) 'set!) (ty:handle-set! expr env))
    
    ;; If
    ((eq? (car expr) 'if) (ty:handle-if expr env))
    
    ;; Begin
    ((eq? (car expr) 'begin) (ty:handle-begin expr env))
    
    ;; Let
    ((eq? (car expr) 'let) (ty:handle-let expr env))
    
    ;; Application
    (else (ty:handle-app expr env))))

;; --- Main Entry Point ---
(define (ty:speculate expr)
  (ty:speculate-inner expr (ty:make-env)))
