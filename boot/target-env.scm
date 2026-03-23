;; copy macro builtins to interaction environment
(copy-environment-macros! (interaction-environment) (current-environment)
  (map car (hashtable->alist (environment-macros (current-environment)))))

;; copy core variables to interaction environment
(copy-environment-variables! (interaction-environment) (current-environment)
  (map car (hashtable->alist (environment-variables (current-environment)))))

#;(copy-environment-variables! (interaction-environment) (current-environment)
'(*
  *cp0-effort-limit*
  *cp0-score-limit*
  *current-context*
  *current-syntax-bindings*
  *current-syntax-meta-env*
  *inlining-depth*
  *mark-counter*
  *modules*
  *rename-env*
  *suffix-counter*
  *syntax-temp-counter*
  *temp-value*
  +
  -
  /
  <
  <=
  =
  >
  >=
  analyze-free-vars-compiler
  analyze-free-vars-optimizer
  analyze-max-outgoing-args
  analyze-mutated-vars-compiler
  analyze-mutated-vars-optimizer
  analyze-params
  analyze-pattern
  analyze-used-vars
  analyze-variable-usage
  any
  any?
  append
  apply
  apply-import-spec
  apply-syntax-rules
  assertion-violation
  assoc
  assq
  assv
  boolean?
  bound-identifier=?
  caaaaar
  caaaadr
  caaaar
  caaadar
  caaaddr
  caaadr
  caaar
  caadaar
  caadadr
  caadar
  caaddar
  caadddr
  caaddr
  caadr
  caar
  cadaaar
  cadaadr
  cadaar
  cadadar
  cadaddr
  cadadr
  cadar
  caddaar
  caddadr
  caddar
  cadddar
  caddddr
  cadddr
  caddr
  cadr
  call-transformer
  call-with-current-continuation
  call-with-values
  call/cc
  call/ec
  car
  cdaaaar
  cdaaadr
  cdaaar
  cdaadar
  cdaaddr
  cdaadr
  cdaar
  cdadaar
  cdadadr
  cdadar
  cdaddar
  cdadddr
  cdaddr
  cdadr
  cdar
  cddaaar
  cddaadr
  cddaar
  cddadar
  cddaddr
  cddadr
  cddar
  cdddaar
  cdddadr
  cdddar
  cddddar
  cdddddr
  cddddr
  cdddr
  cddr
  cdr
  char-numeric?
  char=?
  char?
  codegen
  codegen-and-run
  codegen-application
  codegen-args
  codegen-if
  codegen-lambda
  codegen-let
  codegen-set!
  codegen-symbol
  collect
  collect-pattern-vars
  compile
  compiler-ctx-add-closure!
  compiler-ctx-all-closures
  compiler-ctx-code
  compiler-ctx-emit!
  compiler-ctx-inc-closure-labels!
  compiler-ctx-inc-labels!
  compiler-ctx-mutated
  compiler-ctx-set-code!
  compute-score
  cons
  cons*
  continuation?
  copy-environment-macros!
  copy-environment-variables!
  core-eval
  core-form?
  count-pair
  current-environment
  datum->syntax
  delete
  display
  drop
  drop-elements
  drop-last-cdr
  drop-last-n-pair
  drop-last-pair
  dynamic-wind
  environment-macro-contains?
  environment-macro-ref
  environment-macro-set!
  environment-macros
  environment-variable-contains?
  environment-variable-ref
  environment-variable-set!
  environment-variables
  eq?
  equal-hash
  equal?
  eqv?
  error
  eval-module-body
  eval-transformer-expr
  every
  every?
  exact?
  exit
  expand
  expand-and
  expand-begin
  expand-case
  expand-cond
  expand-define
  expand-define-module
  expand-define-syntax
  expand-if
  expand-import-module
  expand-lambda
  expand-let
  expand-let*
  expand-let*-syntax
  expand-let-syntax
  expand-letrec*
  expand-letrec-syntax
  expand-or
  expand-qq-list-splicing.7_7977a76f-9022-4cac-894c-69694c305f04
  expand-qq-list.8_4be3df6c-235e-4d4b-8957-b5977d4f28f5
  expand-qq.9_0f7949d2-c658-4baf-aad5-9a8432eaa00c
  expand-quasiquote
  expand-quasiquote-form
  expand-quote
  expand-set!
  expand-syntax
  expand-syntax-case
  expand-template
  expand-with-syntax
  extract-library-name
  extract-macro-defs
  extract-module-defined-ids
  extract-quasisyntax
  filter
  filter-runtime-forms
  fixnum?
  flatten-begins
  flatten-params
  flatten-params-ll
  flatten-params-opt
  fold
  for-each
  for-each-1.4_8bbcb2db-4744-46e2-93fd-679a473d22f6
  for-each-n.5_48cf2c30-3be4-4c96-943e-67da2f785b2f
  free-identifier=?
  fresh-mark
  fresh-suffix
  gen-label
  generate-lifted-name
  generate-temporaries
  generate-temporary-symbol
  gensym
  get-identifier-context
  get-param-names
  global-env
  has-effects?
  hashtable->alist
  hashtable-clear!
  hashtable-contains?
  hashtable-delete!
  hashtable-entries
  hashtable-ref
  hashtable-set!
  hashtable?
  identifier?
  inexact?
  infinite?
  inject-binding!
  inst-arg1
  inst-arg2
  inst-op
  integer?
  interaction-environment
  internal-define->binding
  intersect-ll
  iota
  is-quasiquote?.4_304bd49a-0bd5-456f-94cd-9b32d50ebe40
  is-unquote-splicing?.3_5e16d412-bcfa-402e-9581-250975eb9772
  is-unquote?.2_3df5cb64-834a-40c7-81fd-bfa3f4759c87
  lambda-lift
  last-cdr
  last-n-pair
  last-pair
  length
  list
  list->vector
  list-head
  list-ref
  list-tail
  list-transpose
  list-transpose+
  list?
  lookup
  lookup-builtin-handler
  lookup-macro
  lookup-module
  loop.24_0cb28299-c22f-42f2-a4c7-ba3b91f7c182
  loop.28_89018374-ec5e-4a97-b3d9-b78d475faa32
  loop.29_3726dfae-3cb6-4a43-b494-37df7c3b98c2
  loop.2_00eef52c-b21c-4584-81ba-bd02aaebc0a9
  loop.2_2f813c6d-b12e-47ae-a2d1-a3e92fd3e8c5
  loop.2_34fdcf5e-2fe0-45ce-a0e0-21263a2ebb62
  loop.2_437c521c-f0f3-45fe-8e29-d4f03b3e843e
  loop.2_5aebf644-97e0-462b-b750-676848007d6b
  loop.2_5b0c7d99-de8b-4757-af48-9ff972a2ea78
  loop.2_60c39238-c87d-4e65-8c9c-1ca8e8934856
  loop.2_850c8e91-3fd8-4baa-8af5-c0eaec41b4a0
  loop.2_8a5d59f7-e502-4250-afd3-c63ff7f47305
  loop.2_a46177ee-5144-4c1f-8927-7a583a6e1e67
  loop.3_2d2c74b8-d7bf-4e6d-9e6b-4dfc1894297f
  loop.3_6d55c369-7026-4263-911d-5cf855c8da50
  loop.3_7ad6c885-e1aa-4222-98b3-2cac6604d2b3
  macro-binding?
  macroexpand
  macroexpand-1
  make-compiler-context
  make-environment
  make-eq-hashtable
  make-equal-hashtable
  make-eqv-hashtable
  make-macro-binding
  make-parameter
  make-reg
  make-seq
  make-syntax-object
  make-syntax-rules-transformer
  make-variable-transformer
  make-vector
  mangle-name
  map
  map-1.4_82031ebf-4be4-414e-b5a9-7a000ffd9745
  map-improper
  map-n.5_91e7eb0e-4ba6-4dff-be58-aa06219c6def
  match-ellipsis
  match-pattern
  max
  member
  memq
  memv
  min
  module-name->string
  move-args
  nan?
  newline
  no-splicing?.5_ddb2705f-af8c-4f4a-a972-34d71d993c42
  not
  null?
  number->string
  number?
  ops-loads
  ops-memory-pairs
  opt-app
  opt-begin
  opt-if
  opt-lambda
  opt-let
  opt-let-inner
  optimize
  optimize-inner
  optimize-once
  pair?
  parameter-proc-0.3_4ea87ef3-e1b5-421d-89c5-0731ea3d2b41
  parameter-proc-1.4_f8ca29af-2c35-45df-a562-73a521cec50f
  parse-transformer
  partition
  perform-inlining
  prepare-eval-expr
  procedure?
  process-export-specs
  process-import-set
  proper-list?
  pure-primitives
  real?
  reconstruct-params
  register-module!
  register-renamed!
  remove-from-list
  rename-symbol
  resolve-core-form
  resolve-identifier
  resolve-variable
  reverse
  safe-to-inline-val?
  safepoint
  set-car!
  set-cdr!
  set-minus
  set-union
  should-inline?
  small-procedure?
  split-internal-defines
  string->number
  string->symbol
  string-append
  string-join
  string-length
  string-ref
  string=?
  string?
  strip-renames
  strip-suffix
  substitute
  substitute-many
  substitute-proc
  substring
  symbol->string
  symbol?
  syntax->datum
  syntax->list
  syntax-case-match
  syntax-depth-map
  syntax-object-context
  syntax-object-datum
  syntax-object?
  syntax-violation
  system-environment
  take
  take-elements
  transpose-matches
  try-drop-lambda
  undefined
  undefined?
  unique
  unquote-tail?.6_61d825d5-e7ae-46b3-9a49-68b1cf67ecf5
  unrename-core
  unspecified
  unspecified?
  unwrap-env
  unwrap-macro-binding
  uuid
  values
  variable-transformer-procedure
  variable-transformer?
  vector
  vector->list
  vector-length
  vector-ref
  vector-set!
  vector?
  write))

#;(copy-environment-variables! (interaction-environment) (current-environment)
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
    
    macroexpand core-eval expand-syntax-case))

; (current-environment (interaction-environment))
