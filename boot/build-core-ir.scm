(cond-expand
  (gauche
   (use srfi-9)
   (add-load-path "." :relative)
   (add-load-path "../core" :relative))
  (ypsilon
   (import (srfi 9))
   (add-load-path "./core")
   (add-load-path "./meta"))
  (else))

(load "core.scm")  ; Load core system

(define (gen-core-ir-each input-file)
  (define (process exp)
    (let* ((expanded (macroexpand exp))
           (optimized (optimize expanded))
           (lifted (lambda-lift optimized))
           (code (compile lifted)))
      code))
  (with-input-from-file input-file
    (lambda ()
      (let loop ((output '()) (input (read)))
        (if (eof-object? input)
            (reverse output)
            (loop (cons (process input) output) (read)))))))

(define (gen-core-ir-batch input-file)
  (define (process exp)
    (let* ((expanded (macroexpand exp))
           (optimized (optimize expanded))
           (lifted (lambda-lift optimized))
           (code (compile lifted)))
      code))
  (with-input-from-file input-file
    (lambda ()
      (list (process (cons 'begin
                       (let loop ((input-list '()))
                         (let ((input (read)))
                           (if (eof-object? input)
                               (reverse input-list)
                               (loop (cons input input-list)))))))))))

;; macro dependency injection
(with-input-from-file "boot/host-macro.scm"
  (lambda ()
        (let loop ((input (read)))
        (if (not (eof-object? input))
            (begin
              (macroexpand input)
              (loop (read)))))))

;; compile core to ir
(define source-files
  '("boot/target-prelude.scm"
    "core/common.scm"
    "core/quasiquote.scm"
    "core/syntax-rules.scm"
    "core/syntax-case.scm"
    "core/macroexpand.scm"
    "core/optimizer.scm"
    "core/lambda-lift.scm"
    "core/compiler.scm"
    "core/eval.scm"
    "boot/target-env.scm"))

(with-output-to-file "boot/core.ir" 
  (lambda ()
    (map 
      (lambda (source) 
        (for-each
          (lambda (x) (write x) (newline))
          (gen-core-ir-each source)))
      source-files)))