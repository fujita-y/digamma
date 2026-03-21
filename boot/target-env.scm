;; copy macro builtins to interaction environment
(copy-environment-macros! (interaction-environment) (current-environment)
  (map car (hashtable->alist (environment-macros (current-environment)))))

;; copy core variables to interaction environment
(copy-environment-variables! (interaction-environment) (current-environment)
  '(+ - * / = < > <= >=
    cons car cdr caar cdar cadr cddr cadar cddar caddar cdddar caddr cdddr cadddr
    set-car! set-cdr! length list cons*
    list-ref list->vector memq memv member assq assv assoc reverse append
    not boolean? char? eq? equal? eqv? exact? inexact? infinite? integer? fixnum?
    list? null? number? nan? pair? procedure? real? string? symbol? vector?
    undefined unspecified undefined? unspecified?

    vector make-vector vector-length vector-ref vector-set! vector->list

    string-length string-ref string=? string-append substring
    symbol->string string->symbol number->string string->number
    char=? char-numeric? max min

    write display newline

    hashtable? equal-hash make-eq-hashtable make-eqv-hashtable make-equal-hashtable
    hashtable-ref hashtable-set! hashtable-delete! hashtable-contains?
    hashtable-clear! hashtable-entries hashtable->alist

    make-environment copy-environment-variables! copy-environment-macros!
    environment-macros environment-variables current-environment
    environment-macro-set! environment-macro-ref environment-macro-contains?
    environment-variable-set! environment-variable-ref environment-variable-contains?
    interaction-environment system-environment

    values call-with-values collect safepoint gensym uuid exit
    error apply call/ec dynamic-wind continuation?
    call/cc call-with-current-continuation
    
    map for-each filter make-parameter every? any?
    
    macroexpand))

; (current-environment (interaction-environment))
