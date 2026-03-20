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

(define (every? pred lis1 . lists)
  (let ((lists (cons lis1 lists)))
    (if (null? (cdr lists))
        ;; Single list case
        (let lp ((l (car lists)))
          (cond ((null? l) #t)
                ((null? (cdr l)) (pred (car l)))
                ((pred (car l)) (lp (cdr l)))
                (else #f)))
        ;; Multi-list case
        (let lp ((l lists))
          (cond ((null? (car l)) #t) ; Any list empty?
                ((null? (cadar l)) ; Is this the last element?
                 (apply pred (map car l)))
                ((apply pred (map car l))
                 (lp (map cdr l)))
                (else #f))))))

(begin
  (define drop-last-cdr
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? lst) (cons (car lst) (loop (cdr lst))))
                     (else '())))))))
  (define drop-last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst))))
                     (else '())))))))
  (define last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (loop (cdr lst)))
                     (else lst)))))))
  (define last-cdr
    (lambda (lst)
      (cond ((pair? lst)
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (loop (cdr lst)))
                     (else (cdr lst)))))
            (else lst))))
  (define count-pair
    (lambda (lst)
      (let loop ((lst lst) (n 0))
        (cond ((pair? lst) (loop (cdr lst) (+ n 1)))
              (else n)))))
  (define last-n-pair
    (lambda (n lst)
      (let ((m (count-pair lst)))
        (cond ((< m n) '())
              (else (list-tail lst (- m n)))))))
  (define drop-last-n-pair
    (lambda (n lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst) (m (- (count-pair lst) n)))
               (cond ((<= m 0) '())
                     ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst) (- m 1))))
                     (else '()))))))))
