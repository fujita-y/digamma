;;; prelude for target implementation

(define map
  (lambda (proc lst1 . lst2)
    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (proc (car lst))
                     (map-1 proc (cdr lst)))))))
    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (apply proc (car lst))
                     (map-n proc (cdr lst)))))))
    (if (null? lst2)
        (if (list? lst1)
            (map-1 proc lst1)
            (error 'map "expected proper list" (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n proc lst)))
              (else
               (error 'map "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define for-each
  (lambda (proc lst1 . lst2)
    (define for-each-1
      (lambda (proc lst)
        (if (null? lst)
            (unspecified)
            (begin (proc (car lst)) (for-each-1 proc (cdr lst))))))
    (define for-each-n
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else (apply proc (car lst)) (for-each-n proc (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (for-each-1 proc lst1)
            (error 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2) => (lambda (lst) (for-each-n proc lst)))
              (else (error 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define every
  (lambda (proc lst1 . lst2)
    (define every-1
      (lambda (proc lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (if (null? rest) (proc head) (and (proc head) (loop (car rest) (cdr rest))))))))
    (define every-n
      (lambda (proc lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (if (null? rest) (apply proc head) (and (apply proc head) (loop (car rest) (cdr rest))))))))
    (if (null? lst2) (every-1 proc lst1) (every-n proc (apply list-transpose* lst1 lst2)))))
    
(define any
  (lambda (proc lst1 . lst2)
    (define any-1
      (lambda (proc lst)
        (cond ((null? lst) #f)
              (else
                (let loop ((head (car lst)) (rest (cdr lst)))
                  (if (null? rest) (proc head) (or (proc head) (loop (car rest) (cdr rest)))))))))
    (define any-n
      (lambda (proc lst)
        (cond ((null? lst) #f)
              (else
                (let loop ((head (car lst)) (rest (cdr lst)))
                  (if (null? rest) (apply proc head) (or (apply proc head) (loop (car rest) (cdr rest)))))))))
    (if (null? lst2) (any-1 proc lst1) (any-n proc (apply list-transpose* lst1 lst2)))))

(define make-parameter
  (lambda (init . rest)
    (define parameter-proc-0
      (lambda ()
        (let ((value #f))
          (lambda args
            (if (null? args)
                value
                (set! value (car args)))))))
    (define parameter-proc-1
      (lambda (converter)
        (if converter
            (let ((value #f))
              (lambda args
                (cond ((null? args) value)
                      ((or (null? (cdr args)) (cadr args))
                      (set! value (converter (car args))))
                      (else
                        (set! value (car args))))))
            (parameter-proc-0))))
    (let ((param
            (cond ((null? rest)
                   (parameter-proc-0))
                  ((null? (cdr rest))
                   (parameter-proc-1 (car rest)))
                  (else
                   (error 'make-parameter "wrong number of arguments" (cons* init rest))))))
      (begin (param init) param))))

(define call-with-port
  (lambda (port proc)
    (call-with-values
      (lambda () (proc port))
      (lambda args
        (close-port port)
        (apply values args)))))

(define call-with-string-output-port
  (lambda (proc)
    (call-with-values
      (lambda () (open-string-output-port))
      (lambda (port extract)
        (proc port)
        (let ((result (extract))) (close-port port) result)))))

(define scheme-load-paths (make-parameter '()))

(define add-load-path
  (lambda (path)
    (cond ((string? path)
           (or (string=? path "")
               (member path (scheme-load-paths))
               (scheme-load-paths (cons path (scheme-load-paths))))
           (scheme-load-paths))
          (else (assertion-violation 'add-load-path (format "expected string, but got ~s" path))))))

(define load
  (lambda (filename . opt-env)
    (let ((env (if (null? opt-env) (current-environment) (car opt-env))))
      (let ((path
              (if (file-exists? filename)
                  filename
                  (any
                    (lambda (prefix)
                      (let ((path (string-append prefix "/" filename)))
                        (and (file-exists? path) path)))
                    (scheme-load-paths)))))
        (if path
            (call-with-port
              (open-file-input-port path)
              (lambda (port)
                (let loop ((expr (read port)))
                  (cond ((eof-object? expr) (unspecified))
                        (else (core-eval expr env) (loop (read port)))))))
            (assertion-violation 'load (format "file ~s not found" filename)))))))

(define load-module
  (lambda (ref)
    (define (path-for ref)
      (string-append
        (apply
          string-append
            (cdr (let loop ((lst ref))
                   (cond ((null? lst) '())
                         ((symbol? (car lst))
                          (cons "/" (cons (symbol->string (car lst)) (loop (cdr lst)))))
                         ((number? (car lst))
                          (cons "/" (cons (number->string (car lst)) (loop (cdr lst)))))
                         (else (loop (cdr lst)))))))
        ".scm"))
    (if (assoc ref (current-module-registry))
        (unspecified)
        (load (path-for ref)))))
