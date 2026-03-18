
;; Register-based VM for Scheme
;;
;; Bytecode Instruction Set:
;;   (nop)                                 - No operation
;;   (const <reg> <idx>)                   - Load constant at <idx> into <reg>
;;   (mov <dst-reg> <src-reg>)             - Move value from <src-reg> to <dst-reg>
;;   (if <t-label> <f-label>)              - Jump to <t-label> if r0 is true, else <f-label>
;;   (global-ref <reg> <var>)              - Load global variable <var> into <reg>
;;   (global-set! <var> <reg>)             - Store <reg> into global variable <var>
;;   (jump <label>)                        - Unconditional jump to <label>
;;   (call <proc-reg> <n-args>)            - Call procedure in <proc-reg> with <n-args> arguments
;;   (tail-call <proc-reg> <n-args>)       - Tail call procedure in <proc-reg> with <n-args> arguments
;;   (ret)                                 - Return from procedure (result in r0)
;;   (make-closure <dst-reg> <label> <free-indices> <fixed-argc> <has-rest?>)
;;                                         - Create closure at <label> capturing free vars from <free-indices>
;;   (closure-ref <reg> <idx>)             - Load free variable at env[<idx>] into <reg>
;;   (closure-cell-ref <reg> <idx>)        - Load value from cell at env[<idx>] into <reg>
;;   (closure-set! <idx> <reg>)            - Store <reg> into free variable at env[<idx>]
;;   (closure-cell-set! <idx> <reg>)       - Store <reg> into cell at env[<idx>]
;;   (reg-cell-ref <dst-reg> <src-reg>)    - Extract value from cell in <src-reg> into <dst-reg>
;;   (reg-cell-set! <dst-reg> <src-reg>)   - Store <src-reg> into cell in <dst-reg>
;;   (make-cell <reg>)                     - Create cell from <reg> value and store back in <reg>
;;   (closure-self <reg>)                  - Load current closure into <reg>

(define-record-type <context>
  (make-context code pc stack cl regs)
  context?
  (code    ctx-code    ctx-code-set!)
  (pc      ctx-pc      ctx-pc-set!)
  (stack   ctx-stack   ctx-stack-set!)
  (cl      ctx-cl      ctx-cl-set!)
  (regs    ctx-regs    ctx-regs-set!))

(define (vm:decode-reg reg)
  (if (fixnum? reg)
      reg
      (if (symbol? reg)
          (let ((s (symbol->string reg)))
            (if (and (> (string-length s) 1) (char=? (string-ref s 0) #\r))
                (string->number (substring s 1 (string-length s)))
                (error "Invalid register name" reg)))
          (error "Invalid register" reg))))

(define (vm:reg-ref ctx reg)
  (vector-ref (ctx-regs ctx) (vm:decode-reg reg)))

(define (vm:reg-set! ctx reg val)
  (vector-set! (ctx-regs ctx) (vm:decode-reg reg) val))

(define-record-type <nanos:closure>
  (make-nanos:closure-vec label free code fixed-argc has-rest)
  nanos:closure?
  (label nanos:closure-label)
  (free  nanos:closure-free)
  (code  nanos:closure-code)
  (fixed-argc nanos:closure-fixed-argc)
  (has-rest   nanos:closure-has-rest))

(define-record-type cell>
  (make-cell-box value)
  cell?
  (value cell-value cell-value-set!))

(define (vm:init-context code)
  (make-context code 0 '() #f (make-vector 64 #f)))

(define *vm:continue* (list 'continue))

;; (nop)
(define (vm:op-nop ctx inst) *vm:continue*)

;; (const <reg> <idx>)
(define (vm:op-const ctx inst)
  (let ((reg (vector-ref inst 1))
        (idx (vector-ref inst 2)))
    (vm:reg-set! ctx reg (vector-ref (ctx-code ctx) idx))
    *vm:continue*))

;; (mov <dst> <src>)
(define (vm:op-mov ctx inst)
  (let ((dst (vector-ref inst 1))
        (src (vector-ref inst 2)))
    (vm:reg-set! ctx dst (vm:reg-ref ctx src))
    *vm:continue*))

;; (if <t-label> <f-label>)
(define (vm:op-if ctx inst)
  (let ((t-label (vector-ref inst 1))
        (f-label (vector-ref inst 2)))
    (if (vm:reg-ref ctx 0)
        (ctx-pc-set! ctx t-label)
        (ctx-pc-set! ctx f-label))
    *vm:continue*))

;; (global-ref <reg> <var>)
(define (vm:op-global-ref ctx inst)
  (let ((reg (vector-ref inst 1))
        (var (vector-ref inst 2)))
    (vm:reg-set! ctx reg (environment-variable-ref var))
    *vm:continue*))

;; (global-set! <var> <src>)
(define (vm:op-global-set! ctx inst)
  (let ((var (vector-ref inst 1))
        (src (vector-ref inst 2)))
    (environment-variable-set! var (vm:reg-ref ctx src))
    *vm:continue*))

;; (jump <label>)
(define (vm:op-jump ctx inst)
  (let ((label (vector-ref inst 1)))
    (ctx-pc-set! ctx label)
    *vm:continue*))

(define (vm:fetch-args ctx n-args)
  (let arg-loop ((i 0))
    (if (= i n-args)
        '()
        (cons (vm:reg-ref ctx i)
              (arg-loop (+ i 1))))))

(define (vm:fetch-args-range ctx start end)
  (let arg-loop ((i start))
    (if (= i end)
        '()
        (cons (vm:reg-ref ctx i)
              (arg-loop (+ i 1))))))

;; (call <proc-reg> <n-args>)
(define (vm:op-call ctx inst)
  (let ((proc-reg (vector-ref inst 1))
        (n-args (vector-ref inst 2)))
    (let ((proc (vm:reg-ref ctx proc-reg)))
      (if (nanos:closure? proc)
          (let ((fixed (nanos:closure-fixed-argc proc))
                (rest? (nanos:closure-has-rest proc)))
            (if (if rest? (< n-args fixed) (not (= n-args fixed)))
                (error "Wrong number of arguments" n-args fixed rest?)
                (begin
                  (if rest?
                      (let ((rest-args (vm:fetch-args-range ctx fixed n-args)))
                        (vm:reg-set! ctx fixed rest-args)))
                  (ctx-stack-set! ctx (cons (list (ctx-pc ctx) (ctx-cl ctx) (vector-copy (ctx-regs ctx)) (ctx-code ctx)) (ctx-stack ctx)))
                  (ctx-cl-set! ctx proc)
                  (ctx-code-set! ctx (nanos:closure-code proc))
                  (ctx-pc-set! ctx (nanos:closure-label proc))
                  *vm:continue*)))
          (let ((args (vm:fetch-args ctx n-args))
                (saved-regs (vector-copy (ctx-regs ctx))))
            (let ((res (apply proc args)))
              (ctx-regs-set! ctx saved-regs)
              (vm:reg-set! ctx 0 res)
              *vm:continue*))))))

;; (tail-call <proc-reg> <n-args>)
(define (vm:op-tail-call ctx inst)
  (let ((proc-reg (vector-ref inst 1))
        (n-args (vector-ref inst 2)))
    (let ((proc (vm:reg-ref ctx proc-reg)))
      (if (nanos:closure? proc)
          (let ((fixed (nanos:closure-fixed-argc proc))
                (rest? (nanos:closure-has-rest proc)))
            (if (if rest? (< n-args fixed) (not (= n-args fixed)))
                (error "Wrong number of arguments" n-args fixed rest?)
                (begin
                  (if rest?
                      (let ((rest-args (vm:fetch-args-range ctx fixed n-args)))
                        (vm:reg-set! ctx fixed rest-args)))
                  (ctx-cl-set! ctx proc)
                  (ctx-code-set! ctx (nanos:closure-code proc))
                  (ctx-pc-set! ctx (nanos:closure-label proc))
                  *vm:continue*)))
          (let ((args (vm:fetch-args ctx n-args)))
            (let ((res (apply proc args)))
              (if (null? (ctx-stack ctx))
                  res
                  (let ((ctx-data (car (ctx-stack ctx))))
                    (ctx-pc-set! ctx (car ctx-data))
                    (ctx-cl-set! ctx (cadr ctx-data))
                    (ctx-regs-set! ctx (caddr ctx-data))
                    (ctx-code-set! ctx (cadddr ctx-data))
                    (ctx-stack-set! ctx (cdr (ctx-stack ctx)))
                    (vm:reg-set! ctx 0 res)
                    *vm:continue*))))))))

;; (ret)
(define (vm:op-ret ctx inst)
  (if (null? (ctx-stack ctx))
      (vm:reg-ref ctx 0)
      (let ((ctx-data (car (ctx-stack ctx)))
            (res (vm:reg-ref ctx 0)))
        (ctx-pc-set! ctx (car ctx-data))
        (ctx-cl-set! ctx (cadr ctx-data))
        (ctx-regs-set! ctx (caddr ctx-data))
        (ctx-code-set! ctx (cadddr ctx-data))
        (ctx-stack-set! ctx (cdr (ctx-stack ctx)))
        (vm:reg-set! ctx 0 res)
        *vm:continue*)))

;; (make-closure <dst> <label> <free-indices> <fixed-argc> <has-rest?>)
(define (vm:op-make-closure ctx inst)
  (let ((dst (vector-ref inst 1))
        (label (vector-ref inst 2))
        (free-indices (vector-ref inst 3))
        (fixed-argc (vector-ref inst 4))
        (has-rest (vector-ref inst 5)))
    (let ((free-vals (map (lambda (idx) (vm:reg-ref ctx idx)) free-indices)))
      (let ((cl (make-nanos:closure-vec label (list->vector free-vals) (ctx-code ctx) fixed-argc has-rest)))
        (vm:reg-set! ctx dst cl))
      *vm:continue*)))

;; (closure-ref <reg> <idx>)
(define (vm:op-closure-ref ctx inst)
  (let ((reg (vector-ref inst 1))
        (idx (vector-ref inst 2)))
    (vm:reg-set! ctx reg (vector-ref (nanos:closure-free (ctx-cl ctx)) idx))
    *vm:continue*))

;; (closure-cell-ref <reg> <idx>)
(define (vm:op-closure-cell-ref ctx inst)
  (let ((reg (vector-ref inst 1))
        (idx (vector-ref inst 2)))
    (let ((val (vector-ref (nanos:closure-free (ctx-cl ctx)) idx)))
      (let ((extracted (if (cell? val) (cell-value val) val)))
        (vm:reg-set! ctx reg extracted))
      *vm:continue*)))

;; (closure-set! <idx> <src>)
(define (vm:op-closure-set! ctx inst)
  (let ((idx (vector-ref inst 1))
        (src (vector-ref inst 2)))
    (vector-set! (nanos:closure-free (ctx-cl ctx)) idx (vm:reg-ref ctx src))
    *vm:continue*))

;; (closure-cell-set! <idx> <src>)
(define (vm:op-closure-cell-set! ctx inst)
  (let ((idx (vector-ref inst 1))
        (src (vector-ref inst 2)))
    (let ((cell (vector-ref (nanos:closure-free (ctx-cl ctx)) idx))
          (val (vm:reg-ref ctx src)))
      (cell-value-set! cell val)
      *vm:continue*)))

;; (reg-cell-ref <dst> <src>)
(define (vm:op-reg-cell-ref ctx inst)
  (let ((dst-reg (vector-ref inst 1))
        (src-reg (vector-ref inst 2)))
    (let ((val (vm:reg-ref ctx src-reg)))
      (let ((extracted (if (cell? val) (cell-value val) val)))
        (vm:reg-set! ctx dst-reg extracted))
      *vm:continue*)))

;; (reg-cell-set! <dst> <src>)
(define (vm:op-reg-cell-set! ctx inst)
  (let ((dst-reg (vector-ref inst 1))
        (src (vector-ref inst 2)))
    (let ((cell (vm:reg-ref ctx dst-reg))
          (val (vm:reg-ref ctx src)))
      (cell-value-set! cell val)
      *vm:continue*)))

;; (make-cell <src> <dst>)
(define (vm:op-make-cell ctx inst)
  (let ((reg (vector-ref inst 1)))
    (let ((cell (make-cell-box (vm:reg-ref ctx reg))))
      (vm:reg-set! ctx reg cell))
    *vm:continue*))

;; (closure-self <dst>)
(define (vm:op-closure-self ctx inst)
  (let ((dst (vector-ref inst 1)))
    (vm:reg-set! ctx dst (ctx-cl ctx))
    *vm:continue*))

(define (vm:vm-run ctx)
  (let loop ()
    (let* ((code (ctx-code ctx))
           (pc (ctx-pc ctx)))
      (if (>= pc (vector-length code))
          (vm:reg-ref ctx 0)
          (let* ((inst (vector-ref code pc))
                 (op (vector-ref inst 0)))
            (ctx-pc-set! ctx (+ pc 1))
            (let ((res (case op
                         ((nop)               (vm:op-nop ctx inst))
                         ((const)             (vm:op-const ctx inst))
                         ((mov)               (vm:op-mov ctx inst))
                         ((if)                (vm:op-if ctx inst))
                         ((global-ref)        (vm:op-global-ref ctx inst))
                         ((global-set!)       (vm:op-global-set! ctx inst))
                         ((jump)              (vm:op-jump ctx inst))
                         ((call)              (vm:op-call ctx inst))
                         ((tail-call)         (vm:op-tail-call ctx inst))
                         ((ret)               (vm:op-ret ctx inst))
                         ((make-closure)      (vm:op-make-closure ctx inst))
                         ((closure-ref)       (vm:op-closure-ref ctx inst))
                         ((closure-cell-ref)  (vm:op-closure-cell-ref ctx inst))
                         ((closure-set!)      (vm:op-closure-set! ctx inst))
                         ((closure-cell-set!) (vm:op-closure-cell-set! ctx inst))
                         ((reg-cell-ref)      (vm:op-reg-cell-ref ctx inst))
                         ((reg-cell-set!)     (vm:op-reg-cell-set! ctx inst))
                         ((make-cell)         (vm:op-make-cell ctx inst))
                         ((closure-self)      (vm:op-closure-self ctx inst))
                         (else (error "Unknown instruction" inst)))))
              (if (eq? res *vm:continue*)
                  (loop)
                  res)))))))

