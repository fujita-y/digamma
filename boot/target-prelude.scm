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
            (assertion-violation 'map "expected proper list" (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n proc lst)))
              (else
               (assertion-violation 'map "expected same length proper lists" (cons* proc lst1 lst2)))))))

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
            (assertion-violation 'for-each "expected proper list" (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2) => (lambda (lst) (for-each-n proc lst)))
              (else (assertion-violation 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))))))

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

(define fold
  (lambda (proc seed lst1 . lst2)
    (define fold-left-1
      (lambda (proc seed lst)
        (cond ((null? lst) seed) (else (fold-left-1 proc (proc seed (car lst)) (cdr lst))))))
    (define fold-left-n
      (lambda (proc seed lst)
        (cond ((null? lst) seed)
              (else (fold-left-n proc (apply proc (append (list seed) (car lst))) (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (fold-left-1 proc seed lst1)
            (assertion-violation 'fold "expected proper list" (cons* proc seed lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2) => (lambda (lst) (fold-left-n proc seed lst)))
              (else (assertion-violation 'fold "expected same length proper lists" (cons* proc seed lst1 lst2)))))))

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

(define call-with-input-file
  (lambda (string proc)
    (call-with-port (open-file-input-port string) proc)))

(define call-with-output-file
  (lambda (string proc)
    (call-with-port (open-file-output-port string) proc)))

;; Tail-recursive filter.
(define (filter pred lst)
  (let loop ((lst lst) (acc '()))
    (cond ((null? lst) (reverse acc))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc)))
          (else (loop (cdr lst) acc)))))

;; Left-associative fold.
(define (fold proc seed lst)
  (let loop ((lst lst) (acc seed))
    (if (null? lst)
        acc
        (loop (cdr lst) (proc (car lst) acc)))))

;; Returns a list of integers from 0 to n-1.
(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;; Partition lst into two lists: those that satisfy pred and those that do not.
(define (partition pred lst)
  (let loop ((lst lst) (in '()) (out '()))
    (cond ((null? lst) (values (reverse in) (reverse out)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) in) out))
          (else (loop (cdr lst) in (cons (car lst) out))))))

(define scheme-load-paths (make-parameter '()))

(define home-directory
  (lambda ()
    (let ((path (or (lookup-process-environment "HOME") "")))
      (and (file-exists? path) path))))

(define expand-path
  (lambda (path)
    (cond ((and (>= (string-length path) 2) 
                (eqv? (string-ref path 0) #\~)
                (eqv? (string-ref path 1) #\/))
           (string-append (or (home-directory) "") (substring path 1 (string-length path))))
          (else path))))

(define add-load-path
  (lambda (path)
    (let ((path (expand-path path)))
      (cond ((string? path)
             (or (string=? path "")
                 (member path (scheme-load-paths))
                 (scheme-load-paths (cons path (scheme-load-paths))))
             (scheme-load-paths))
            (else (assertion-violation 'add-load-path (format "expected string, but got ~s" path)))))))

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
                (let loop ((expr '()))
                  (let ((obj (read port)))
                    (cond ((eof-object? obj) 
                           (core-eval (cons 'begin (reverse expr)) env)
                           (unspecified))
                         (else (loop (cons obj expr))))))))
            (assertion-violation 'load (format "file ~s not found in ~s" filename (scheme-load-paths))))))))

(define load-module
  (lambda (lib-name)
    (define (path-for lib-name)
      (string-append
        (apply
          string-append
            (cdr (let loop ((lst lib-name))
                   (cond ((null? lst) '())
                         ((symbol? (car lst))
                          (cons "/" (cons (symbol->string (car lst)) (loop (cdr lst)))))
                         ((number? (car lst))
                          (cons "/" (cons (number->string (car lst)) (loop (cdr lst)))))
                         (else (loop (cdr lst)))))))
        ".scm"))
    (if (assoc lib-name (current-module-registry))
        (unspecified)
        (load (path-for lib-name)))))
