(load "types.scm")

;; --- Test Helper Functions ---

(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expr expected)
  (let ((result (ty:speculate expr)))
    (if (equal? result expected)
        (begin
          (set! *pass-count* (+ *pass-count* 1))
          (display "PASS: ") (display name) (newline))
        (begin
          (set! *fail-count* (+ *fail-count* 1))
          (display "FAIL: ") (display name) (newline)
          (display "  input:    ") (write expr) (newline)
          (display "  expected: ") (write expected) (newline)
          (display "  actual:   ") (write result) (newline)))))

(display "Running Type Speculation Tests...\n")
(newline)

;; --- Basic Literal Tests ---

(test "Number literal - no annotation"
      '42
      '42)

(test "String literal - no annotation"
      '"hello"
      '"hello")

(test "Boolean literal - no annotation"
      '#t
      '#t)

;; --- Variable Tests ---

(test "Unknown variable gets annotated with 'any' type (skipped)"
      'x
      'x)  ;; We don't annotate 'any type

;; --- Let Binding Tests ---

(test "Let with cons - variable gets pair type"
      '(let ((x (cons 1 2))) x)
      '(let ((x (cons 1 2))) (begin 'type:pair x)))

(test "Let with number - variable gets number type"
      '(let ((x (+ 1 2))) x)
      '(let ((x (+ 1 2))) (begin 'type:number x)))

(test "Let with multiple bindings"
      '(let ((x (cons 1 2)) (y (+ 3 4))) (list x y))
      '(let ((x (cons 1 2)) (y (+ 3 4))) 
         (list (begin 'type:pair x) (begin 'type:number y))))

(test "Nested let bindings"
      '(let ((x (cons 1 2))) (let ((y (car x))) y))
      '(let ((x (cons 1 2))) 
         (let ((y (car (begin 'type:pair x)))) 
           y)))  ;; y is type 'any from car

;; --- Primitive Operation Tests ---

(test "cons produces pair - no annotation on compound expr"
      '(cons 1 2)
      '(cons 1 2))

(test "car with variable"
      '(let ((x (cons 1 2))) (car x))
      '(let ((x (cons 1 2))) (car (begin 'type:pair x))))

(test "cdr with variable"
      '(let ((x (cons 1 2))) (cdr x))
      '(let ((x (cons 1 2))) (cdr (begin 'type:pair x))))

(test "Arithmetic with variables"
      '(let ((x 5) (y 10)) (+ x y))
      '(let ((x 5) (y 10)) (+ (begin 'type:number x) (begin 'type:number y))))

(test "String operation"
      '(let ((s "hello")) (string-length s))
      '(let ((s "hello")) (string-length (begin 'type:string s))))

(test "Vector operation"
      '(let ((v (vector 1 2 3))) (vector-length v))
      '(let ((v (vector 1 2 3))) (vector-length (begin 'type:vector v))))

;; --- Type Predicate Tests ---

(test "pair? with variable"
      '(let ((x (cons 1 2))) (pair? x))
      '(let ((x (cons 1 2))) (pair? (begin 'type:pair x))))

(test "number? with variable"
      '(let ((x 42)) (number? x))
      '(let ((x 42)) (number? (begin 'type:number x))))

;; --- Control Flow Tests ---

(test "Simple if - no type refinement"
      '(if #t 1 2)
      '(if #t 1 2))

(test "If with pair? predicate - type refinement in then branch"
      '(let ((x (cons 1 2))) (if (pair? x) (car x) 0))
      '(let ((x (cons 1 2))) 
         (if (pair? (begin 'type:pair x)) 
             (car (begin 'type:pair x)) 
             0)))

(test "If with number? predicate"
      '(let ((x 42)) (if (number? x) (+ x 1) 0))
      '(let ((x 42)) 
         (if (number? (begin 'type:number x)) 
             (+ (begin 'type:number x) 1) 
             0)))

(test "If with type predicate on unknown variable"
      '(if (pair? y) (car y) 0)
      '(if (pair? y) (car (begin 'type:pair y)) 0))

;; --- Begin Tests ---

(test "Begin with multiple expressions"
      '(begin (display "hello") (+ 1 2))
      '(begin (display "hello") (+ 1 2)))

(test "Begin with variable"
      '(let ((x 42)) (begin (display x) x))
      '(let ((x 42)) 
         (begin (display (begin 'type:number x)) 
                (begin 'type:number x))))

;; --- Lambda Tests ---

(test "Lambda - parameters have unknown type"
      '(lambda (x) x)
      '(lambda (x) x))  ;; x has type 'any, not annotated

(test "Lambda with body using parameter"
      '(lambda (x) (+ x 1))
      '(lambda (x) (+ x 1)))  ;; x is 'any

(test "Let with lambda"
      '(let ((f (lambda (x) x))) f)
      '(let ((f (lambda (x) x))) (begin 'type:procedure f)))

;; --- Define Tests ---

(test "Define variable"
      '(begin (define x 10) x)
      '(begin (define x 10) (begin 'type:number x)))

(test "Define function"
      '(begin (define (f x) x) f)
      '(begin (define (f x) x) (begin 'type:procedure f)))

(test "Define lambda variable"
      '(begin (define g (lambda (x) x)) g)
      '(begin (define g (lambda (x) x)) (begin 'type:procedure g)))

(test "Define string variable"
      '(begin (define s "hello") s)
      '(begin (define s "hello") (begin 'type:string s)))

(test "Define vector variable"
      '(begin (define v (vector 1 2 3)) v)
      '(begin (define v (vector 1 2 3)) (begin 'type:vector v)))

;; --- Quote Tests ---

(test "Quoted pair"
      '(let ((x '(1 2 3))) x)
      '(let ((x '(1 2 3))) (begin 'type:pair x)))

(test "Quoted number"
      '(let ((x '42)) x)
      '(let ((x '42)) (begin 'type:number x)))

(test "Quoted symbol"
      '(let ((x 'foo)) x)
      '(let ((x 'foo)) (begin 'type:symbol x)))

;; --- Complex Tests ---

(test "Chained car/cdr operations"
      '(let ((x (cons 1 (cons 2 3)))) (car (cdr x)))
      '(let ((x (cons 1 (cons 2 3)))) (car (cdr (begin 'type:pair x)))))

(test "Multiple variable uses"
      '(let ((x (cons 1 2))) (cons x x))
      '(let ((x (cons 1 2))) (cons (begin 'type:pair x) (begin 'type:pair x))))

(test "Nested if with type refinement"
      '(let ((x '(1 2))) 
         (if (pair? x) 
             (if (number? (car x)) 
                 (car x) 
                 0) 
             0))
      '(let ((x '(1 2))) 
         (if (pair? (begin 'type:pair x)) 
             (if (number? (car (begin 'type:pair x))) 
                 (car (begin 'type:pair x)) 
                 0) 
             0)))

(test "Set! updates variable type"
      '(let ((x 42)) (begin (set! x "hello") x))
      '(let ((x 42)) 
         (begin (set! x "hello") 
                (begin 'type:string x))))

;; --- R7RS Primitive Coverage ---

(test "List operations"
      '(let ((lst (list 1 2 3))) (length lst))
      '(let ((lst (list 1 2 3))) (length (begin 'type:pair lst))))

(test "Empty list returns null (not pair)"
      '(let ((lst (list))) lst)
      '(let ((lst (list))) (begin 'type:null lst)))

(test "List with single argument returns pair"
      '(let ((lst (list 'x))) lst)
      '(let ((lst (list 'x))) (begin 'type:pair lst)))

(test "List with variable argument returns pair"
      '(let ((x 42)) (let ((lst (list x))) lst))
      '(let ((x 42)) (let ((lst (list (begin 'type:number x)))) (begin 'type:pair lst))))

;; --- Collection Conversion Tests ---

(test "string->list with empty string returns null"
      '(let ((lst (string->list ""))) lst)
      '(let ((lst (string->list ""))) (begin 'type:null lst)))

(test "string->list with non-empty string returns pair"
      '(let ((lst (string->list "abc"))) lst)
      '(let ((lst (string->list "abc"))) (begin 'type:pair lst)))

(test "vector->list with empty vector returns null"
      '(let ((lst (vector->list #()))) lst)
      '(let ((lst (vector->list #()))) (begin 'type:null lst)))

(test "vector->list with non-empty vector returns pair"
      '(let ((lst (vector->list #(1 2 3)))) lst)
      '(let ((lst (vector->list #(1 2 3)))) (begin 'type:pair lst)))

(test "append with no arguments returns null"
      '(let ((lst (append))) lst)
      '(let ((lst (append))) (begin 'type:null lst)))

(test "append with all null literals returns null"
      '(let ((lst (append '() '()))) lst)
      '(let ((lst (append '() '()))) (begin 'type:null lst)))

(test "reverse with null returns null"
      '(let ((lst (reverse '()))) lst)
      '(let ((lst (reverse '()))) (begin 'type:null lst)))

(test "String operations"
      '(let ((s (string-append "hello" " " "world"))) s)
      '(let ((s (string-append "hello" " " "world"))) (begin 'type:string s)))

(test "Symbol operations"
      '(let ((sym (string->symbol "foo"))) sym)
      '(let ((sym (string->symbol "foo"))) (begin 'type:symbol sym)))

(test "Boolean operations"
      '(let ((b (not #f))) b)
      '(let ((b (not #f))) (begin 'type:boolean b)))

(test "Comparison operations"
      '(let ((cmp (< 1 2))) cmp)
      '(let ((cmp (< 1 2))) (begin 'type:boolean cmp)))

(test "No error on car can assume its argument is a pair"
      '(lambda (x) (let ((a (car x))) (let ((b (cdr x))) (list a b x))))
      '(lambda (x) (let ((a (car x))) (let ((b (cdr (begin 'type:pair x)))) (list a b (begin 'type:pair x))))))

;; --- Character Operation Tests ---

(test "char->integer produces number"
      '(let ((n (char->integer #\a))) n)
      '(let ((n (char->integer #\a))) (begin 'type:number n)))

(test "integer->char produces char"
      '(let ((c (integer->char 97))) c)
      '(let ((c (integer->char 97))) (begin 'type:char c)))

(test "char-upcase produces char"
      '(let ((c (char-upcase #\a))) c)
      '(let ((c (char-upcase #\a))) (begin 'type:char c)))

;; --- Bytevector Operation Tests ---

(test "make-bytevector produces bytevector"
      '(let ((bv (make-bytevector 10))) bv)
      '(let ((bv (make-bytevector 10))) (begin 'type:bytevector bv)))

(test "bytevector produces bytevector"
      '(let ((bv (bytevector 1 2 3))) bv)
      '(let ((bv (bytevector 1 2 3))) (begin 'type:bytevector bv)))

(test "bytevector-length produces number"
      '(let ((bv (bytevector 1 2 3))) (bytevector-length bv))
      '(let ((bv (bytevector 1 2 3))) (bytevector-length (begin 'type:bytevector bv))))

(test "bytevector-u8-ref produces number"
      '(let ((bv (bytevector 1 2 3))) (bytevector-u8-ref bv 0))
      '(let ((bv (bytevector 1 2 3))) (bytevector-u8-ref (begin 'type:bytevector bv) 0)))

;; --- More Complex Control Flow Tests ---

(test "Nested if with different types"
      '(if (number? x) (if (pair? y) y 1) "string")
      '(if (number? x) (if (pair? y) (begin 'type:pair y) 1) "string"))

(test "If with else branch only - predicate refinement"
      '(if (string? s) (string-length s) 0)
      '(if (string? s) (string-length (begin 'type:string s)) 0))

;; --- Edge Cases ---

(test "Empty quote - null"
      '(let ((x '())) x)
      '(let ((x '())) (begin 'type:null x)))

(test "Multiple set! with different types"
      '(let ((x 1)) (begin (set! x "two") (set! x 'three) x))
      '(let ((x 1)) (begin (set! x "two") (set! x 'three) (begin 'type:symbol x))))

(test "Lambda with multiple parameters"
      '(lambda (x y z) (+ x y))
      '(lambda (x y z) (+ x y)))

(test "Lambda with dotted parameter list"
      '(lambda (x . rest) x)
      '(lambda (x . rest) x))

(test "Nested lambdas"
      '(lambda (x) (lambda (y) (+ x y)))
      '(lambda (x) (lambda (y) (+ x y))))

(test "Lambda returns lambda"
      '(let ((f (lambda (x) (lambda (y) y)))) f)
      '(let ((f (lambda (x) (lambda (y) y)))) (begin 'type:procedure f)))

;; --- More Primitive Coverage ---

(test "exact->inexact produces number"
      '(let ((n (exact->inexact 5))) n)
      '(let ((n (exact->inexact 5))) (begin 'type:number n)))

(test "sqrt produces number"
      '(let ((n (sqrt 16))) n)
      '(let ((n (sqrt 16))) (begin 'type:number n)))

(test "list->vector produces vector"
      '(let ((v (list->vector '(1 2 3)))) v)
      '(let ((v (list->vector '(1 2 3)))) (begin 'type:vector v)))

(test "vector->list produces pair"
      '(let ((lst (vector->list (vector 1 2 3)))) lst)
      '(let ((lst (vector->list (vector 1 2 3)))) lst))

(test "string->list produces pair"
      '(let ((lst (string->list "abc"))) lst)
      '(let ((lst (string->list "abc"))) (begin 'type:pair lst)))

(test "list->string produces string"
      '(let ((s (list->string '(#\a #\b #\c)))) s)
      '(let ((s (list->string '(#\a #\b #\c)))) (begin 'type:string s)))

;; --- Complex Scenarios ---

(test "Flow-sensitive inference with car argument"
      '(let ((result (car x))) result)
      '(let ((result (car x))) result))

(test "Multiple uses of same variable with different operations"
      '(let ((x (cons 1 2))) (begin (car x) (cdr x) x))
      '(let ((x (cons 1 2))) (begin (car (begin 'type:pair x)) (cdr (begin 'type:pair x)) (begin 'type:pair x))))

(test "Type refinement persists through begin"
      '(let ((x (+ 1 2))) (begin (display x) (+ x 5)))
      '(let ((x (+ 1 2))) (begin (display (begin 'type:number x)) (+ (begin 'type:number x) 5))))

(test "Comparison operators produce boolean"
      '(let ((b (>= 5 3))) b)
      '(let ((b (>= 5 3))) (begin 'type:boolean b)))

(test "Vector-ref produces any type"
      '(let ((v (vector 1 2 3))) (let ((x (vector-ref v 0))) x))
      '(let ((v (vector 1 2 3))) (let ((x (vector-ref (begin 'type:vector v) 0))) x)))

(test "String-ref produces char"
      '(let ((s "hello")) (let ((c (string-ref s 0))) c))
      '(let ((s "hello")) (let ((c (string-ref (begin 'type:string s) 0))) (begin 'type:char c))))

(test "Multiple type predicates in sequence"
      '(if (pair? x) (if (null? y) 1 2) 3)
      '(if (pair? x) (if (null? y) 1 2) 3))

(test "Apply type predicate to result of expression"
      '(let ((x (cons 1 2))) (if (pair? x) (car x) #f))
      '(let ((x (cons 1 2))) (if (pair? (begin 'type:pair x)) (car (begin 'type:pair x)))))

;; --- Procedure Type Tests ---

(test "Procedure predicate"
      '(let ((f (lambda (x) x))) (if (procedure? f) f #f))
      '(let ((f (lambda (x) x))) (if (procedure? (begin 'type:procedure f)) (begin 'type:procedure f))))

;; --- Chained Pair Tests ---

(test "Chained caaar implies car is pair"
      '(let ((v (caaar x))) (car x))
      '(let ((v (caaar x))) (car (begin 'type:pair x))))

(test "Chained caaar implies caar is pair"
      '(let ((v (caaar x))) (caar x))
      '(let ((v (caaar x))) (caar (begin 'type:pair x))))

(test "Chained caadr implies car is pair"
      '(let ((v (caadr x))) (car x))
      '(let ((v (caadr x))) (car (begin 'type:pair x))))

(test "Chained caaar implies nested car access is pair"
      '(let ((v (caaar x))) (let ((y (car x))) (let ((z (car y))) z)))
      '(let ((v (caaar x))) 
         (let ((y (car (begin 'type:pair x)))) 
           (let ((z (car (begin 'type:pair y)))) 
             (begin 'type:pair z)))))

(test "Chained caaar do not imply cdr is pair"
      '(let ((v (caaar x))) (let ((y (cdr x))) (let ((z (cdr y))) z)))
      '(let ((v (caaar x))) (let ((y (cdr (begin 'type:pair x)))) (let ((z (cdr y))) z))))

(test "Recursive procedure"
      '(define loop (lambda (x) (if (pair? x) (loop (cdr x)))))
      '(define loop (lambda (x) (if (pair? x) ((begin 'type:procedure loop) (cdr (begin 'type:pair x)))))))

(test "Recursive procedure (let)"
      '((let ((loop #f)) (set! loop (lambda (i) (if (= i 10) #t (loop (+ i 1)))) loop) 1))
      '((let ((loop #f)) (set! loop (lambda (i) (if (= i 10) #t ((begin 'type:procedure loop) (+ (begin 'type:number i) 1))))) 1)))

;; --- Display Results ---
(newline)
(if (= *fail-count* 0)
    (display "ALL TESTS PASSED.\n")
    (begin
      (display "FAILED ") (display *fail-count*) (display " TESTS.\n")))
(newline)
