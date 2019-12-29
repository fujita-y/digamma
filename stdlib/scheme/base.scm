(define-library
  (scheme base)
  (import (except (core) for-each map)
          (except (rnrs) for-each map))
  (export *
          +
          -
          ...
          /
          <
          <=
          =
          =>
          >
          >=
          _
          abs
          and
          append
          apply
          assoc
          assq
          assv
          begin
          binary-port?
          boolean=?
          boolean?
          bytevector
          bytevector-append
          bytevector-copy
          bytevector-copy!
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector?
          caar
          cadr
          call-with-current-continuation
          call-with-port
          call-with-values
          call/cc
          car
          case
          cdar
          cddr
          cdr
          ceiling
          char->integer
          char-ready?
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          close-input-port
          close-output-port
          close-port
          complex?
          cond
          cond-expand
          cons
          current-error-port
          current-input-port
          current-output-port
          define
          define-record-type
          define-syntax
          define-values
          denominator
          do
          dynamic-wind
          else
          eof-object
          eof-object?
          eq?
          equal?
          eqv?
          error
          error-object-irritants
          error-object-message
          error-object?
          even?
          exact
          exact-integer-sqrt
          exact-integer?
          exact?
          expt
          features
          file-error?
          floor
          floor-quotient
          floor-remainder
          floor/
          flush-output-port
          for-each
          gcd
          get-output-bytevector
          get-output-string
          guard
          if
          include
          include-ci
          inexact
          inexact?
          input-port-open?
          input-port?
          integer->char
          integer?
          lambda
          lcm
          length
          let
          let*
          let*-values
          let-syntax
          let-values
          letrec
          letrec*
          letrec-syntax
          list
          list->string
          list->vector
          list-copy
          list-ref
          list-set!
          list-tail
          list?
          make-bytevector
          make-list
          make-parameter
          make-string
          make-vector
          map
          max
          member
          memq
          memv
          min
          modulo
          negative?
          newline
          not
          null?
          number->string
          number?
          numerator
          odd?
          open-input-bytevector
          open-input-string
          open-output-bytevector
          open-output-string
          or
          output-port-open?
          output-port?
          pair?
          parameterize
          peek-char
          peek-u8
          port?
          positive?
          procedure?
          quasiquote
          quote
          quotient
          raise
          raise-continuable
          rational?
          rationalize
          read-bytevector
          read-bytevector!
          read-char
          read-error?
          read-line
          read-string
          read-u8
          real?
          remainder
          reverse
          round
          set!
          set-car!
          set-cdr!
          square
          string
          string->list
          string->number
          string->symbol
          string->utf8
          string->vector
          string-append
          string-copy
          string-copy!
          string-fill!
          string-for-each
          string-length
          string-map
          string-ref
          string-set!
          string<=?
          string<?
          string=?
          string>=?
          string>?
          string?
          substring
          symbol->string
          symbol=?
          symbol?
          syntax-error
          syntax-rules
          textual-port?
          truncate
          truncate-quotient
          truncate-remainder
          truncate/
          u8-ready?
          unless
          unquote
          unquote-splicing
          utf8->string
          values
          vector
          vector->list
          vector->string
          vector-append
          vector-copy
          vector-copy!
          vector-fill!
          vector-for-each
          vector-length
          vector-map
          vector-ref
          vector-set!
          vector?
          when
          with-exception-handler
          write-bytevector
          write-char
          write-string
          write-u8
          zero?)
  (begin
    (define for-each-1
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else
                (proc (car lst))
                (for-each-1 proc (cdr lst))))))

    (define for-each-n
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else
                (apply proc (car lst))
                (for-each-n proc (cdr lst))))))

    (define for-each
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (for-each-1 proc lst1)
            (for-each-n proc (apply list-transpose* lst1 lst2)))))

    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (proc (car lst))
                          (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (apply proc (car lst))
                          (map-n proc (cdr lst)))))))

    (define map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (map-1 proc lst1)
            (map-n proc (apply list-transpose* lst1 lst2)))))

    (define bytevector
      (lambda args
        (u8-list->bytevector args)))

    (define bytevector-append
      (lambda args
        (let ((ans (make-bytevector (apply + (map bytevector-length args)))))
          (let loop ((args args) (p 0))
              (or (null? args)
                  (let ((n (bytevector-length (car args))))
                    (bytevector-copy! (car args) 0 ans p n)
                    (loop (cdr args) (+ p n)))))
          ans)))

    (define char-ready?
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (not (eof-object? (lookahead-char port))))))

    (define cond-expand #f)

    (define-syntax define-values
      (lambda (x)
        (syntax-case x ()
          ((stx (formals ...) expression)
           (with-syntax (((n ...) (datum->syntax #'k (iota (length #'(formals ...))))))
            #'(begin
                (define temp (call-with-values (lambda () expression) vector))
                (define formals (vector-ref temp n)) ...))))))

    (define error-object? #f)
    (define error-object-irritants #f)
    (define error-object-message #f)
    (define exact-integer? #f)
    (define features #f)
    (define file-error? #f)
    (define floor/ #f)
    (define floor-quotient #f)
    (define floor-remainder #f)
    (define get-output-bytevector #f)
    (define get-output-string #f)
    (define include-ci #f)
    (define include #f)
    (define input-port-open? #f)
    (define list-set! #f)
    (define open-input-bytevector #f)
    (define open-input-string #f)
    (define open-output-bytevector #f)
    (define open-output-string #f)
    (define output-port-open? #f)
    (define peek-u8 #f)
    (define read-bytevector #f)
    (define read-bytevector! #f)
    (define read-error? #f)
    (define read-line #f)
    (define read-string #f)
    (define read-u8 #f)
    (define square #f)
    (define string-copy! #f)
    (define string-map #f)
    (define string->vector #f)
    (define syntax-error #f)
    (define truncate/ #f)
    (define truncate-quotient #f)
    (define truncate-remainder #f)
    (define u8-ready? #f)
    (define vector-append #f)
    (define vector-copy! #f)
    (define vector->string #f)
    (define write-bytevector #f)
    (define write-string #f)
    (define write-u8 #f)
  )
) ;[end]
