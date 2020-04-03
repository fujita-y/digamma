(begin
  (define |ok?`2*|
    (lambda (|row`13*| |dist`13*| |placed`13*|)
      (if (null? |placed`13*|)
          #t
          (and (not (= (car |placed`13*|) (+ |row`13*| |dist`13*|)))
               (not (= (car |placed`13*|) (- |row`13*| |dist`13*|)))
               (|ok?`2*| |row`13*| (+ |dist`13*| 1) (cdr |placed`13*|))))))
  (define |1-to`2*| (lambda (|n`3*|) (|loop`7*| |n`3*| '())))
  (define |loop`7*|
    (lambda (|i`8*| |l`8*|) (if (= |i`8*| 0) |l`8*| (|loop`7*| (- |i`8*| 1) (cons |i`8*| |l`8*|)))))
  (define |my-try`2*|
    (lambda (|x`11*| |y`11*| |z`11*|)
      (if (null? |x`11*|)
          (if (null? |y`11*|) 1 0)
          (+
            (if (|ok?`2*| (car |x`11*|) 1 |z`11*|)
                (|my-try`2*| (append (cdr |x`11*|) |y`11*|) '() (cons (car |x`11*|) |z`11*|))
                0)
            (|my-try`2*| (cdr |x`11*|) (cons (car |x`11*|) |y`11*|) |z`11*|)))))
  (define nqueens (lambda (|n`1*|) (|my-try`2*| (|1-to`2*| |n`1*|) '() '()))))