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

(define (gen-core-ir input-file)
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

;; macro dependency injection
;; parameterize
;; let-values

(define source-files
  '("boot/prelude.scm"
    "core/common.scm"
    "core/quasiquote.scm"
    "core/syntax-rules.scm"
    "core/syntax-case.scm"
    "core/macroexpand.scm"
    "core/optimizer.scm"
    "core/lambda-lift.scm"
    "core/compiler.scm"))

(with-output-to-file "boot/core.ir" 
  (lambda ()
    (map 
      (lambda (source) 
        (for-each
          (lambda (x) (write x) (newline))
          (gen-core-ir source)))
      source-files)))