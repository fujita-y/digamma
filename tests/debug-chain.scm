(define (gentemp) (car (generate-temporaries '(x))))

(define (id=? x y) (and (identifier? x) (free-identifier=? x y)))

(define-syntax chain
  (lambda (x)
    (syntax-case x ()
      ((_ initial-value (step ...) ...)
        #'(chain initial-value _ (... ...) (step ...) ...))
      ((_ initial-value placeholder (step ...) ...)
        #'(chain initial-value placeholder (... ...) (step ...) ...))
      ((_ initial-value placeholder ellipsis) (and (identifier? #'placeholder)
                                                   (identifier? #'ellipsis))
        #'initial-value)
      ((_ initial-value placeholder ellipsis (step ...) rest ...)
        (let loop ((vars '()) (out '()) (in #'(step ...)))
          (syntax-case in ()
            ((u ?...) (and (id=? #'u #'placeholder) (id=? #'?... #'ellipsis))
              (let ((chain-rest-var (gentemp)))
                #`(chain (let-values ((#,(if (null? vars)
                                           chain-rest-var
                                           #`(#,@(reverse vars) . #,chain-rest-var))
                                       initial-value))
                           (apply #,@(reverse out) #,chain-rest-var))
                         placeholder
                         ellipsis
                         rest ...)))
            ((u ?... . _) (and (id=? #'u #'placeholder) (id=? #'?... #'ellipsis))
              (syntax-violation 'chain "_ ... only allowed at end" #'(step ...)))
            ((u . step-rest) (id=? #'u #'placeholder)
              (let ((chain-var (gentemp)))
                (loop (cons chain-var vars) (cons chain-var out) #'step-rest)))
            ((?... . _) (id=? #'?... #'ellipsis)
              (syntax-violation 'chain "misplaced ..." #'(step ...)))
            ((x . step-rest)
              (loop vars (cons #'x out) #'step-rest))
            (()
              (with-syntax ((result (reverse out)))
                #`(chain
                    #,(cond
                        ((null? vars)
                          #'(begin initial-value result))
                        ((null? (cdr vars))
                          #`(let ((#,(car vars) initial-value)) result))
                        (else
                          #`(let-values ((#,(reverse vars) initial-value)) result)))
                    placeholder
                    ellipsis
                    rest ...)))))))))

(write (chain ""
         (string-append "foo" _)
         (string-append "bar" _)
         (string-append "baz" _)
         )
)
(newline)
(exit)
