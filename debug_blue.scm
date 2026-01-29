
(define (repl:load-relative dir file)
  (let ((old-env (sys-getcwd)))
    (sys-chdir dir)
    (load (string-append "./" file))
    (sys-chdir old-env)))

(let ((root (sys-getcwd)))
  (repl:load-relative (string-append root "/research/syntax") "macroexpand.scm")
  (repl:load-relative (string-append root "/research/opt") "optimizer.scm"))

(let ((expr '(begin
               (define-syntax blue
                 (syntax-rules ()
                   ((blue x)
                    (let-syntax ((red (syntax-rules ()
                                        ((red y) (list x y)))))
                      (red 'z)))))
               (let ((list (lambda (x y) 'captured)))
                 (blue 'w)))))
  (let* ((expanded (macroexpand expr)))
    (format #t "Expanded: ~s\n" expanded)))
