(define-module research.vm.vm
  (use srfi-1)
  (use research.vm.compiler) ;; To ensure we can test together, though VM doesn't depend on compiler logic, just output.
  (export make-vm vm-load vm-run register-subr vm-stack)
  )
(select-module research.vm.vm)

;; --- VM Structure ---

(define-class <vm> ()
  ((registers :init-form (make-vector 1024) :accessor vm-registers)
   (stack     :init-form '() :accessor vm-stack) ;; Call stack (pc, fp, closure, dst-reg)
   (pc        :init-form '() :accessor vm-pc)
   (fp        :init-form 0   :accessor vm-fp)
   (closure   :init-form #f  :accessor vm-closure)
   (globals   :init-form (make-hash-table) :accessor vm-globals)
   (output    :init-form '() :accessor vm-output))) ;; For testing display/output

(define (make-vm)
  (make <vm>))

(define (vm-error vm msg . args)
  (errorf "VM Error: ~a ~a" msg args))

;; --- Global Environment ---

(define (register-subr vm name proc)
  (hash-table-put! (vm-globals vm) name proc))

(define (vm-global-ref vm name)
  (hash-table-get (vm-globals vm) name #f))

(define (vm-global-set! vm name val)
  (hash-table-put! (vm-globals vm) name val))

;; --- Closure Structure ---

(define-class <closure> ()
  ((code :init-keyword :code :accessor closure-code)
   (free-vars :init-keyword :free-vars :accessor closure-free-vars)))

(define (make-flat-closure code free-vars)
  (make <closure> :code code :free-vars (list->vector free-vars)))

;; --- Instruction Processing (Linking) ---

(define (link-code instructions)
  ;; Convert list of insts to vector for fast indexing (pc as index)
  ;; Resolve labels to indices
  (let* ((len (length instructions))
         (vec (list->vector instructions))
         (labels (make-hash-table)))
    ;; Pass 1: find labels
    (let loop ((i 0))
      (if (< i len)
          (let ((inst (vector-ref vec i)))
            (if (symbol? inst)
                (hash-table-put! labels inst i))
            (loop (+ i 1)))))
    ;; Pass 2: resolve jumps and remove labels (or just skip them)
    ;; We keep labels in vector but jumps point to next index?
    ;; Easier: Replace labels with `(nop)` and jumps with indices.
    (let ((new-vec (make-vector len)))
      (let loop ((i 0))
        (if (< i len)
            (let ((inst (vector-ref vec i)))
              (cond
               ((symbol? inst)
                (vector-set! new-vec i '(nop)))
               ((pair? inst)
                (let ((op (car inst)))
                  (cond
                   ((eq? op 'jump)
                    (let ((target (cadr inst)))
                      (vector-set! new-vec i `(jump ,(hash-table-get labels target)))))
                   ((eq? op 'if)
                    (let ((reg (cadr inst))
                          (t-lab (caddr inst))
                          (f-lab (cadddr inst)))
                      (vector-set! new-vec i `(if ,reg ,(hash-table-get labels t-lab) ,(hash-table-get labels f-lab)))))
                   ((eq? op 'make-closure)
                    ;; Code in make-closure is a list (nested). Need to link it too.
                    (let ((dst (cadr inst))
                          (code (caddr inst))
                          (frees (cdddr inst)))
                      (vector-set! new-vec i `(make-closure ,dst ,(link-code code) ,@frees))))
                   ((eq? op 'tail-call)
                    (vector-set! new-vec i inst)) ;; No label resolution needed for tail-call operands
                   (else
                    (vector-set! new-vec i inst)))))
               (else (vector-set! new-vec i inst)))
              (loop (+ i 1)))))
      new-vec)))

(define (vm-load vm code)
  (set! (vm-pc vm) 0)
  (set! (vm-fp vm) 0)
  ;; Link code
  (let ((linked (link-code code)))
    ;; Wrap in a top-level closure?
    ;; Or just execute.
    (set! (vm-closure vm) (make-flat-closure linked '()))
    ;;(print "Loaded code: " linked)
    ))

;; --- Execution ---

(define (vm-run vm)
  (let loop ()
    (let* ((cl (vm-closure vm))
           (code (closure-code cl))
           (pc (vm-pc vm))
           (inst (if (< pc (vector-length code)) (vector-ref code pc) #f)))
      
      ;(print "PC: " pc " INST: " inst " FP: " (vm-fp vm))
      
      (if (not inst)
          (vm-error vm "PC out of bounds")
          (let ((op (car inst)))
            (cond
             ((eq? op 'nop)
              (set! (vm-pc vm) (+ pc 1))
              (loop))
             
             ((eq? op 'const)
              (let ((dst (cadr inst))
                    (val (caddr inst)))
                (vector-set! (vm-registers vm) (+ (vm-fp vm) dst) val)
                (set! (vm-pc vm) (+ pc 1))
                (loop)))
             
             ((eq? op 'mov)
              (let ((dst (cadr inst))
                    (src (caddr inst)))
                (vector-set! (vm-registers vm) (+ (vm-fp vm) dst)
                             (vector-ref (vm-registers vm) (+ (vm-fp vm) src)))
                (set! (vm-pc vm) (+ pc 1))
                (loop)))
             
             ((eq? op 'gref)
              (let ((dst (cadr inst))
                    (sym (caddr inst)))
                (let ((val (vm-global-ref vm sym)))
                  (if (not val) (error "Unbound global" sym))
                  (vector-set! (vm-registers vm) (+ (vm-fp vm) dst) val)
                  (set! (vm-pc vm) (+ pc 1))
                  (loop))))

             ((eq? op 'gset)
              (let ((sym (cadr inst))
                    (src (caddr inst)))
                (let ((val (vector-ref (vm-registers vm) (+ (vm-fp vm) src))))
                  (vm-global-set! vm sym val)
                  (set! (vm-pc vm) (+ pc 1))
                  (loop))))

             ((eq? op 'jump)
              (let ((target (cadr inst)))
                (set! (vm-pc vm) target)
                (loop)))

             ((eq? op 'if)
              (let ((reg (cadr inst))
                    (t-target (caddr inst))
                    (f-target (cadddr inst)))
                (let ((val (vector-ref (vm-registers vm) (+ (vm-fp vm) reg))))
                  (if val
                      (set! (vm-pc vm) t-target)
                      (set! (vm-pc vm) f-target))
                  (loop))))

             ((eq? op 'make-closure)
              (let ((dst (cadr inst))
                    (sub-code (caddr inst)) ;; Linked vector
                    (frees (cdddr inst)))
                ;; Collect free var values
                (let ((free-vals (map (lambda (reg)
                                        (if (number? reg)
                                            (vector-ref (vm-registers vm) (+ (vm-fp vm) reg))
                                            (error "Invalid free var reg" reg)))
                                      frees)))
                  (let ((new-cl (make-flat-closure sub-code free-vals)))
                    (vector-set! (vm-registers vm) (+ (vm-fp vm) dst) new-cl)
                    (set! (vm-pc vm) (+ pc 1))
                    (loop)))))

             ((eq? op 'closure-ref)
              (let ((dst (cadr inst))
                    (idx (caddr inst)))
                (let ((val (vector-ref (closure-free-vars (vm-closure vm)) idx)))
                  (vector-set! (vm-registers vm) (+ (vm-fp vm) dst) val)
                  (set! (vm-pc vm) (+ pc 1))
                  (loop))))

             ((eq? op 'closure-set!)
              (let ((idx (cadr inst))
                    (src (caddr inst)))
                (let ((val (vector-ref (vm-registers vm) (+ (vm-fp vm) src))))
                  (vector-set! (closure-free-vars (vm-closure vm)) idx val)
                  (set! (vm-pc vm) (+ pc 1))
                  (loop))))

             ((eq? op 'call)
              (let ((dst (cadr inst))
                    (proc-reg (caddr inst))
                    (arg-count (cadddr inst)))
                (let ((proc (vector-ref (vm-registers vm) (+ (vm-fp vm) proc-reg))))
                  (cond
                   ((is-a? proc <closure>)
                    ;; Save state
                    (set! (vm-stack vm) (cons (list (+ pc 1) (vm-fp vm) (vm-closure vm) dst) (vm-stack vm)))
                    ;; Set new state
                    (set! (vm-closure vm) proc)
                    (set! (vm-pc vm) 0)
                    ;; Offset FP. Args are at proc-reg + 1.
                    ;; New FP should be such that new reg 0 is old proc-reg + 1 ?
                    ;; In compiler: `new-env (map cons bound-vars (iota (length bound-vars)))`
                    ;; So arg 0 is at reg 0.
                    ;; In caller, arg 0 is at proc-reg + 1.
                    ;; So new FP = old FP + proc-reg + 1.
                    (set! (vm-fp vm) (+ (vm-fp vm) proc-reg 1))
                    (loop))
                   ((procedure? proc) ;; Subr (Scheme procedure)
                    ;; Args are in registers [proc-reg+1 ... proc-reg+arg-count]
                    (let ((args (let loop-args ((i 0) (acc '()))
                                  (if (= i arg-count) (reverse acc)
                                      (loop-args (+ i 1)
                                                 (cons (vector-ref (vm-registers vm) (+ (vm-fp vm) proc-reg 1 i)) acc))))))
                      (let ((result (apply proc args)))
                        (vector-set! (vm-registers vm) (+ (vm-fp vm) dst) result)
                        (set! (vm-pc vm) (+ pc 1))
                        (loop))))
                   (else
                    (vm-error vm "Not a procedure" proc))))))

             ((eq? op 'tail-call)
              (let ((proc-reg (cadr inst))
                    (arg-count (caddr inst)))
                (let ((proc (vector-ref (vm-registers vm) (+ (vm-fp vm) proc-reg))))
                  (cond
                   ((is-a? proc <closure>)
                    ;; Reuse current frame.
                    ;; Args are at proc-reg + 1 .. proc-reg + arg-count.
                    ;; Move them to 0 .. arg-count - 1.
                    (let loop-move ((i 0))
                      (if (< i arg-count)
                          (begin
                            (vector-set! (vm-registers vm) (+ (vm-fp vm) i)
                                         (vector-ref (vm-registers vm) (+ (vm-fp vm) proc-reg 1 i)))
                            (loop-move (+ i 1)))))
                    ;; Set new state
                    (set! (vm-closure vm) proc)
                    (set! (vm-pc vm) 0)
                    ;; FP remains same!
                    (loop))
                   ((procedure? proc) ;; Subr
                    ;; Collect args
                    (let ((args (let loop-args ((i 0) (acc '()))
                                  (if (= i arg-count) (reverse acc)
                                      (loop-args (+ i 1)
                                                 (cons (vector-ref (vm-registers vm) (+ (vm-fp vm) proc-reg 1 i)) acc))))))
                      (let ((result (apply proc args)))
                         ;; Return logic
                         (if (null? (vm-stack vm))
                             result ;; End of computation
                             (let ((frame (car (vm-stack vm))))
                               (set! (vm-stack vm) (cdr (vm-stack vm)))
                               (let ((ret-pc (car frame))
                                     (ret-fp (cadr frame))
                                     (ret-cl (caddr frame))
                                     (ret-dst (cadddr frame)))
                                 ;; Restore state
                                 (set! (vm-pc vm) ret-pc)
                                 (set! (vm-fp vm) ret-fp)
                                 (set! (vm-closure vm) ret-cl)
                                 ;; Store result
                                 (vector-set! (vm-registers vm) (+ (vm-fp vm) ret-dst) result)
                                 (loop)))))))
                    (else
                     (vm-error vm "Not a procedure" proc))))))

             ((eq? op 'return)
              (let ((res-reg (cadr inst)))
                (let ((result (vector-ref (vm-registers vm) (+ (vm-fp vm) res-reg))))
                  (if (null? (vm-stack vm))
                      result ;; End of computation
                      (let ((frame (car (vm-stack vm))))
                        (set! (vm-stack vm) (cdr (vm-stack vm)))
                        (let ((ret-pc (car frame))
                              (ret-fp (cadr frame))
                              (ret-cl (caddr frame))
                              (ret-dst (cadddr frame)))
                          ;; Restore state
                          (set! (vm-pc vm) ret-pc)
                          (set! (vm-fp vm) ret-fp)
                          (set! (vm-closure vm) ret-cl)
                          ;; Store result
                          (vector-set! (vm-registers vm) (+ (vm-fp vm) ret-dst) result)
                          (loop)))))))

             (else
              (vm-error vm "Unknown opcode" op))))))))

