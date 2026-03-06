(import (srfi 9))
(add-load-path "./core")
(add-load-path "./meta")

(load "core.scm")

(define expr
  '(define map
     (lambda (proc.1 lst1.2 . lst2.3)
       (let ((map-1.4 #f) (map-n.5 #f))
         (set! map-1.4
               (lambda (proc.6 lst.7)
                 (if (null? lst.7) '() (cons (proc.6 (car lst.7)) (map-1.4 proc.6 (cdr lst.7))))))
         (set! map-n.5
               (lambda (proc.8 lst.9)
                 (if (null? lst.9)
                     '()
                     (cons (apply proc.8 (car lst.9)) (map-n.5 proc.8 (cdr lst.9))))))
         (if (null? lst2.3)
             (if (list? lst1.2)
                 (map-1.4 proc.1 lst1.2)
                 (error 'map "expected proper list" (cons* proc.1 lst1.2 lst2.3)))
             (let ((tmp.10.11 (apply list-transpose+ lst1.2 lst2.3)))
               (if tmp.10.11
                   ((lambda (lst.12) (map-n.5 proc.1 lst.12)) tmp.10.11)
                   (error 'map "expected same length proper lists" (cons* proc.1 lst1.2 lst2.3)))))))))

(define (pretty-print x)
  (write x) (newline))

(let* ((expanded (macroexpand expr))
       (optimized (optimize expanded))
       (lifted (lambda-lift optimized)))
  (pretty-print lifted))
