(define (main args)
  (guard (con
           [else
            (write 'got-exception) (newline)])
    (syntax-violation 'foo "bad syntax" '(bar 1)))
	
  (guard (con
           [else
            (write 'got-assertion-exception) (newline)])
    (assertion-violation 'baz "bad assert" 42))
)
