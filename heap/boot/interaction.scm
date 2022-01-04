;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define continuation-to-exit (make-parameter #f))
(define dump-condition (make-parameter #f))
(define self-evaluating-vector-constants (make-parameter #t))
(define ellipsis/underscore-in-literal (make-parameter #t))
(define right-arrow-in-case (make-parameter #t))

(define default-exception-printer
  (lambda (c . out)
    (let ((out (if (pair? out) (car out) (current-error-port))))
      (current-exception-handler #f)
      (let ((port (make-string-output-port)))
        (parameterize ((pretty-print-line-length (backtrace-line-length))
                       (pretty-print-maximum-lines 5)
                       (pretty-print-unwrap-syntax #t))
          (define output-who-message
            (lambda ()
              (format port "error")
              (and (who-condition? c)
                   (if (string? (condition-who c))
                       (format port " in ~a" (condition-who c))
                       (format port " in ~u" (condition-who c))))
              (and (message-condition? c) (format port ": ~a" (condition-message c)))))
          (define output-irritants
            (lambda ()
              (cond
                ((and (irritants-condition? c) (pair? (condition-irritants c)))
                 (format port "~%~%irritants:")
                 (for-each
                   (lambda (e)
                     (format port "~% ")
                     (cond ((list? e)
                            (format port " (")
                            (let loop ((lst (map (lambda (e) (format "~r" e)) e)))
                              (cond
                                ((pair? lst)
                                 (format port "~a" (car lst))
                                 (cond ((pair? (cdr lst)) (format port " ") (loop (cdr lst)))))))
                            (format port ")"))
                           (else (format port " ~r" e))))
                   (condition-irritants c))))))
          (define output-expansion
            (lambda ()
              (and (expansion-backtrace)
                   (current-macro-expression)
                   (parameterize ((pretty-print-initial-indent 5) (pretty-print-maximum-lines 10))
                     (format port "~%~%expanding:~%  >  ")
                     (pretty-print (current-macro-expression) port)
                     (format port "~%  ~n" (current-macro-expression))
                     (and (>= (expansion-trace-level) (expansion-backtrace)) (format port "~%  *  ..."))
                     (for-each
                       (lambda (e) (format port "~%  *  ") (pretty-print e port) (format port "~%  ~n" e))
                       (expansion-trace-stack))))))
          (define output-condition (lambda (c) (and (dump-condition) (format port "~%~%") (describe-condition port c))))
          (cond ((syntax-violation? c)
                 (output-who-message)
                 (cond
                   ((syntax-violation-form c)
                    =>
                    (lambda (form)
                      (or (eq? (and (who-condition? c) (condition-who c)) (syntax->datum form))
                          (parameterize ((pretty-print-initial-indent 5))
                            (format port "~%  >  ")
                            (pretty-print (syntax->datum form) port)
                            (let ((form (if (wrapped-syntax-object? form) (syntax-object-expr form) form)))
                              (and (pair? form) (format port "~%  ~n" form))))))))
                 (cond
                   ((syntax-violation-subform c)
                    =>
                    (lambda (form)
                      (or (eq? (and (who-condition? c) (condition-who c)) (syntax->datum form))
                          (parameterize ((pretty-print-initial-indent 5))
                            (format port "~%  @  ")
                            (pretty-print (syntax->datum form) port)
                            (let ((form (if (wrapped-syntax-object? form) (syntax-object-expr form) form)))
                              (and (pair? form) (format port "~%  ~n" form))))))))
                 (output-condition c)
                 (let ((e1 (syntax-violation-form c)) (e2 (syntax-violation-subform c)))
                   (let ((e1 (if (wrapped-syntax-object? e1) (syntax-object-expr e1) e1))
                         (e2 (if (wrapped-syntax-object? e2) (syntax-object-expr e2) e2)))
                     (or (and (null? (expansion-trace-stack))
                              (or (eq? (current-macro-expression) e1) (eq? (current-macro-expression) e2)))
                         (output-expansion)))))
                ((undefined-violation? c)
                 (format port "error: unbound variable")
                 (and (who-condition? c) (format port " ~u" (condition-who c)))
                 (and (message-condition? c) (format port ", ~a" (condition-message c)))
                 (output-irritants)
                 (output-condition c)
                 (output-expansion))
                ((error? c) (output-who-message) (output-irritants) (output-condition c) (output-expansion))
                ((violation? c) (output-who-message) (output-irritants) (output-condition c) (output-expansion))
                ((warning? c)
                 (format port "warning")
                 (and (who-condition? c) (format port " in ~u" (condition-who c)))
                 (and (message-condition? c) (format port ": ~a" (condition-message c)))
                 (output-irritants)
                 (output-condition c)
                 (output-expansion))
                ((condition? c)
                 (format port "error: unknown type of exception caught~%~%irritants:~%~a" (describe-condition #f c))
                 (output-irritants)
                 (output-expansion))
                (else
                  (format port "error: unknown type of exception caught, ~a" c)
                  (output-irritants)
                  (output-expansion))))
        (format port "~%")
        (let ((plugged (not (eq? (port-device-subtype (current-input-port)) 'char))))
          (and (serious-condition? c) (display-backtrace port))
          (if plugged
              (format out "~a~!" (extract-accumulated-string port))
              (format out "~%~a~%~!" (extract-accumulated-string port))))))))

(define current-exception-printer
  (make-parameter
    default-exception-printer
    (lambda (x)
      (cond ((not x) values)
            ((procedure? x) x)
            (else
              (assertion-violation 'current-exception-printer (format "expected procedure or #f, but got ~s" x)))))))

(define add-load-path
  (lambda (path)
    (cond ((string? path)
           (or (string=? path "")
               (let ((path (expand-path path)))
                 (or (member path (scheme-load-paths)) (scheme-load-paths (cons path (scheme-load-paths))))))
           (scheme-load-paths))
          (else (assertion-violation 'add-load-path (format "expected string, but got ~s" path))))))

(define add-library-path
  (lambda (path)
    (cond ((string? path)
           (or (string=? path "")
               (let ((path (expand-path path)))
                 (or (member path (scheme-library-paths)) (scheme-library-paths (cons path (scheme-library-paths))))))
           (scheme-library-paths))
          (else (assertion-violation 'add-library-path (format "expected string, but got ~s" path))))))

(define home-directory
  (lambda ()
    (let ((path (or (lookup-process-environment "HOME") "")))
      (and (file-exists? path) path))))

(define process (lambda args (apply process-spawn #t #f #f #f #f args)))

(define process-shell-command
  (lambda (command)
    (process (or (getenv "SHELL") "/bin/sh") "-c" command)))

(define apply-scheme-proc-assistant
  (lambda (proc . args)
    (let ((throw #f) (level (recursion-level)))
      (dynamic-wind
        (lambda ()
          (or (= level (recursion-level))
              (assertion-violation #f "scheme continuation interleave with c/c++ continuation"))
          (set! throw #f))
        (lambda ()
          (with-exception-handler
            (lambda (c) (set! throw #t) (raise c))
            (lambda () (apply proc args))))
        (lambda () (and throw (escape)))))))

(define nonblock-skip-whitespace
  (lambda ()
    (and (nonblock-byte-ready? (current-input-port))
         (let ((ch (lookahead-char (current-input-port))))
           (and (not (eof-object? ch)) (char-whitespace? ch)))
         (get-char (current-input-port))
         (nonblock-skip-whitespace))))

(define nonblock-input-wait
  (lambda ()
    (parameterize ((collect-stack-notify #f))
      (let loop ()
        (cond ((nonblock-byte-ready? (current-input-port)))
              (else (usleep 10000) (loop)))))))

(define read-eval-print-loop
  (lambda ()
    (let ((plugged (not (eq? (port-device-subtype (current-input-port)) 'char))))
      (let loop ()
        (call-with-current-continuation
          (lambda (continue)
            (with-exception-handler
              (lambda (c)
                (flush-output-port (current-output-port))
                ((current-exception-printer) c)
                (and (serious-condition? c) (continue)))
              (lambda ()
                (nonblock-skip-whitespace)
                (if (eq? (current-environment) (interaction-environment))
                    (format #t "~&> ~!")
                    (format #t "~&~a: ~!" (current-environment)))
                (nonblock-input-wait)
                (current-macro-expression #f)
                (current-source-comments (make-core-hashtable))
                (current-temporaries (make-core-hashtable 'string=?))
                (set-port-current-line! (current-input-port) 1)
                (set-port-current-column! (current-output-port) 1)
                (set-port-current-column! (current-error-port) 1)
                (let ((form (core-read (current-input-port) (current-source-comments) 'read)))
                  (and (eof-object? form) (exit 0))
                  (and plugged (format #t "~%~!"))
                  (let ((ans (interpret form)))
                    (cond ((unspecified? ans))
                          (else (pretty-print ans) (flush-output-port (current-output-port))))))))))
        (loop)))))

(define quiet-read-eval-print-loop
  (lambda ()
    (let loop ()
      (call-with-current-continuation
        (lambda (continue)
          (with-exception-handler
            (lambda (c)
              (flush-output-port (current-output-port))
              ((current-exception-printer) c)
              (and (serious-condition? c) (exit #f)))
            (lambda ()
              (nonblock-skip-whitespace)
              (nonblock-input-wait)
              (current-macro-expression #f)
              (current-source-comments (make-core-hashtable))
              (current-temporaries (make-core-hashtable 'string=?))
              (let ((form (core-read (current-input-port) (current-source-comments) 'read)))
                (cond ((eof-object? form) (exit 0))
                      (else (interpret form) (flush-output-port (current-output-port)))))))))
      (loop))))

(define display-warning
  (lambda (message form subform)
    (let ((port (make-string-output-port)))
      (format port "~a" message)
      (parameterize ((pretty-print-line-length (backtrace-line-length))
                     (pretty-print-maximum-lines 10)
                     (pretty-print-unwrap-syntax #t)
                     (pretty-print-initial-indent 5))
        (cond (form (format port "~%  >  ") (pretty-print form port) (and (pair? form) (format port "~%  ~n" form))))
        (cond
          (subform
            (format port "~%  >  ")
            (pretty-print subform port)
            (and (pair? subform) (format port "~%  ~n" subform))))
        (format port "~%")
        (let ((plugged (not (eq? (port-device-subtype (current-input-port)) 'char))))
          (if plugged
              (format (current-error-port) "~a~!" (extract-accumulated-string port))
              (format (current-error-port) "~%~a~!" (extract-accumulated-string port))))))))

(define start-scheme-session
  (lambda ()
    (let ((status (call/cc (lambda (cont) (continuation-to-exit cont) (run-scheme-session) 0))))
      (exit status))))

(define run-scheme-session
  (lambda ()
    (define directory-exists? (lambda (path) (file-exists? (format "~a/." path))))
    (define init-sys-sitelib
      (lambda ()
        (let ((path (format "~a/sitelib" (system-share-path))))
          (and (directory-exists? path) (add-library-path path)))))
    (define init-sys-acc
      (lambda ()
        (let ((home (home-directory)))
          (and (directory-exists? (format "~//.digamma" home))
               (auto-compile-cache (format "~//.digamma" home))))))
    (define init-env-acc
      (lambda ()
        (cond
          ((lookup-process-environment "DIGAMMA_ACC")
           =>
           (lambda (path)
             (cond ((directory-exists? (expand-path path)) (auto-compile-cache (expand-path path)))
                   (else
                     (format
                       (current-error-port)
                       "** ERROR in environment variable 'DIGAMMA_ACC': directory ~s not exist~%"
                       path)
                     (auto-compile-cache #f))))))))
    (define init-env-sitelib
      (lambda ()
        (cond
          ((lookup-process-environment "DIGAMMA_SITELIB")
           =>
           (lambda (paths)
             (for-each
               (lambda (path)
                 (cond ((directory-exists? (expand-path path)) (add-library-path (expand-path path)))
                       (else
                         (format
                           (current-error-port)
                           "** ERROR in environment variable 'DIGAMMA_SITELIB': directory ~s not exist~%"
                           path))))
               (reverse (string-split paths #\:))))))))
    (define init-env-loadpath
      (lambda ()
        (cond
          ((lookup-process-environment "DIGAMMA_LOADPATH")
           =>
           (lambda (paths)
             (for-each
               (lambda (path)
                 (cond ((directory-exists? (expand-path path)) (add-load-path (expand-path path)))
                       (else
                         (format
                           (current-error-port)
                           "** ERROR in environment variable 'DIGAMMA_LOADPATH': directory ~s not exist~%"
                           path))))
               (reverse (string-split paths #\:))))))))
    (define add-opt-sitelib
      (lambda (paths)
        (for-each
          (lambda (path)
            (cond ((directory-exists? (expand-path path)) (add-library-path (expand-path path)))
                  (else
                    (format
                      (current-error-port)
                      "** ERROR in option '--sitelib=~a': directory ~s not exist~%"
                      paths
                      path)
                    (exit #f))))
          (reverse (string-split paths #\:)))))
    (define add-opt-loadpath
      (lambda (paths)
        (for-each
          (lambda (path)
            (cond ((directory-exists? (expand-path path)) (add-load-path (expand-path path)))
                  (else
                    (format
                      (current-error-port)
                      "** ERROR in option '--loadpath=~a': directory ~s not exist~%"
                      paths
                      path)
                    (exit #f))))
          (reverse (string-split paths #\:)))))
    (define set-opt-acc
      (lambda (path)
        (cond ((directory-exists? (expand-path path)) (auto-compile-cache (expand-path path)))
              (else
                (format (current-error-port) "** ERROR in option '--acc=~a': directory ~s not exist~%" path path)
                (exit #f)))))
    (define bad-option
      (lambda (opt) (format (current-error-port) "** ERROR in option '~a'~%" opt) (show-usage) (exit #f)))
    (define show-usage
      (lambda ()
        (format #t "usage: digamma [options] [--] [file] [arguments]~%")
        (format #t "options:~%")
        (format #t "  --r7rs (-7)            conforms r7rs syntax and semantics~%")
        (format #t "  --r6rs (-6)            conforms r6rs syntax and semantics~%")
        (format #t "  --top-level-program    runs the top-level program~%")
        (format #t "  --mute (-m)            suppresses greeting~%")
        (format #t "  --quiet (-q)           suppresses greeting, repl prompt, and repl output~%")
        (format #t "  --verbose (-v)         prints load and compile activities~%")
        (format #t "  --warning (-w)         prints warnings~%")
        (format #t "  --interactive (-i)     enters repl after running the script file~%")
        (format #t "  --sitelib=path         adds sitelib path (DIGAMMA_SITELIB)~%")
        (format #t "  --loadpath=path        adds load search path (DIGAMMA_LOADPATH)~%")
        (format #t "  --acc=dir              sets a auto-compile-cache directory (DIGAMMA_ACC)~%")
        (format #t "  --heap-limit=mbytes    sets a total heap limit in MBytes~%")
        (format #t "  --dump-condition       default exception handler dumps condition~%")
        (format #t "  --disable-acc          disables auto-compile-cache~%")
        (format #t "  --clean-acc            cleans auto-compile-cache~%")
        (format #t "  --version              prints version and exit~%")
        (format #t "  --help                 prints help and exit~%")
        (format #t "  --                     indicates no more option to proceed~%")))
    (define show-banner (lambda () (format #t "digamma 0.1.~a~%" (architecture-feature 'program-revision))))
    (define show-info
      (lambda ()
        (show-banner)
        (cond ((lookup-process-environment "DIGAMMA_ACC") => (lambda (path) (format #t ";; DIGAMMA_ACC=~a~%" path)))
              (else (format #t ";; DIGAMMA_ACC unspecified~%")))
        (cond ((lookup-process-environment "DIGAMMA_SITELIB")
               =>
               (lambda (path) (format #t ";; DIGAMMA_SITELIB=~a~%" path)))
              (else (format #t ";; DIGAMMA_SITELIB unspecified~%")))
        (cond ((lookup-process-environment "DIGAMMA_LOADPATH")
               =>
               (lambda (path) (format #t ";; DIGAMMA_LOADPATH=~a~%" path)))
              (else (format #t ";; DIGAMMA_LOADPATH unspecified~%")))
        (format #t ";; (auto-compile-cache) => ~s~%" (auto-compile-cache))
        (format #t ";; (scheme-library-paths) => ~s~%" (scheme-library-paths))
        (format #t ";; (scheme-load-paths) => ~s~%" (scheme-load-paths))))
    (define exec-script
      (lambda (lst)
        (command-line-shift (- (length (command-line)) (length lst)))
        (let ((path (car lst)))
          (cond (interaction
                  (with-exception-handler
                    (lambda (c)
                      (flush-output-port (current-output-port))
                      ((current-exception-printer) c)
                      (and (serious-condition? c) (exec-repl)))
                    (lambda () (auto-compile-cache-update) (load path))))
                (else
                  (with-exception-handler
                    (lambda (c)
                      (flush-output-port (current-output-port))
                      ((current-exception-printer) c)
                      (and (serious-condition? c) (exit #f)))
                    (lambda ()
                      (auto-compile-cache-update)
                      (cond ((or top-level-program (load-file-has-r6rs-comment? path))
                             (load-top-level-program path))
                            (else (interpret '(import (core))) (load path)))
                      (flush-output-port (current-error-port))
                      (flush-output-port (current-output-port))
                      (exit))))))))
    (define exec-repl
      (lambda ()
        (cond (mute) (verbose (show-info)) (else (show-banner)))
        (or script (interpret '(import (core))))
        (if quiet (quiet-read-eval-print-loop) (read-eval-print-loop))))
    (define verbose #f)
    (define quiet #f)
    (define interaction #f)
    (define script #f)
    (define mute #f)
    (define top-level-program #f)
    (define initial-command-line (command-line))
    (init-sys-acc)
    (init-env-acc)
    (init-sys-sitelib)
    (init-env-sitelib)
    (init-env-loadpath)
    (let ((lst initial-command-line))
      (and (pair? lst)
           (let loop ((lst (cdr lst)))
             (cond ((null? lst) (if interaction (exec-repl) (or script (exec-repl))))
                   (else
                     (let ((opt (car lst)))
                       (define opt?
                         (lambda (flag rhs?)
                           (let ((n (string-contains opt flag)))
                             (cond ((not n) #f)
                                   ((not (= n 0)) #f)
                                   ((string=? opt flag) "")
                                   ((and rhs? (char=? (string-ref opt (string-length flag)) #\=))
                                    (substring opt (+ (string-length flag) 1) (string-length opt)))
                                   (else #f)))))
                       (cond ((opt? "--heap-limit" #f) (loop (cddr lst)))
                             ((opt? "--heap-limit" #t) (loop (cdr lst)))
                             ((opt? "--no-letrec-check" #f)
                              (format (current-error-port) "** WARNING: '--no-letrec-check' option is deprecated~%")
                              (loop (cdr lst)))
                             ((or (opt? "--warning" #f) (opt? "-w" #f)) (warning-level #t) (loop (cdr lst)))
                             ((opt? "--version" #f) (show-banner) (exit))
                             ((opt? "--help" #f) (show-usage) (exit))
                             ((or (opt? "--r7rs" #f) (opt? "-7" #f))
                              (right-arrow-in-case #t)
                              (ellipsis/underscore-in-literal #t)
                              (self-evaluating-vector-constants #t)
                              (lexical-syntax-version 7)
                              (mutable-literals #f)
                              (loop (cdr lst)))
                             ((or (opt? "--r6rs" #f) (opt? "-6" #f))
                              (right-arrow-in-case #f)
                              (self-evaluating-vector-constants #f)
                              (ellipsis/underscore-in-literal #f)
                              (lexical-syntax-version 6)
                              (mutable-literals #f)
                              (loop (cdr lst)))
                             ((opt? "--top-level-program" #f) (set! top-level-program #t) (loop (cdr lst)))
                             ((or (opt? "--verbose" #f) (opt? "-v" #f))
                              (scheme-load-verbose #t)
                              (auto-compile-verbose #t)
                              (set! verbose #t)
                              (set! mute #f)
                              (loop (cdr lst)))
                             ((or (opt? "--mute" #f) (opt? "-m" #f)) (set! verbose #f) (set! mute #t) (loop (cdr lst)))
                             ((or (opt? "--quiet" #f) (opt? "-q" #f))
                              (set! verbose #f)
                              (set! mute #t)
                              (set! quiet #t)
                              (loop (cdr lst)))
                             ((or (opt? "--interactive" #f) (opt? "-i" #f)) (set! interaction #t) (loop (cdr lst)))
                             ((opt? "--dump-condition" #f) (dump-condition #t) (loop (cdr lst)))
                             ((opt? "--acc" #f)
                              (or (pair? (cdr lst)) (bad-option opt))
                              (set-opt-acc (cadr lst))
                              (loop (cddr lst)))
                             ((opt? "--acc" #t)
                              =>
                              (lambda (rhs)
                                (cond ((string=? rhs "") (bad-option opt)) (else (set-opt-acc rhs) (loop (cdr lst))))))
                             ((opt? "--disable-acc" #f) (auto-compile-cache #f) (loop (cdr lst)))
                             ((opt? "--clean-acc" #f) (auto-compile-cache-clean) (loop (cdr lst)))
                             ((opt? "--sitelib" #f)
                              (or (pair? (cdr lst)) (bad-option opt))
                              (add-opt-sitelib (cadr lst))
                              (loop (cddr lst)))
                             ((opt? "--sitelib" #t)
                              =>
                              (lambda (rhs)
                                (cond ((string=? rhs "") (bad-option opt))
                                      (else (add-opt-sitelib rhs) (loop (cdr lst))))))
                             ((opt? "--loadpath" #f)
                              (or (pair? (cdr lst)) (bad-option opt))
                              (add-opt-loadpath (cadr lst))
                              (loop (cddr lst)))
                             ((opt? "--loadpath" #t)
                              =>
                              (lambda (rhs)
                                (cond ((string=? rhs "") (bad-option opt))
                                      (else (add-opt-loadpath rhs) (loop (cdr lst))))))
                             ((opt? "--" #f) (set! script #t) (exec-script (cdr lst)) (and interaction (exec-repl)))
                             ((char=? (string-ref opt 0) #\-) (bad-option opt))
                             (else (set! script #t) (exec-script lst) (and interaction (exec-repl))))))))))))
