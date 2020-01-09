(define-library (test libtest1)
  (export foo)
  (include-library-declarations "decl.scm")
  (cond-expand (r7rs (include-ci "bar.scm" "foo.scm"))))
