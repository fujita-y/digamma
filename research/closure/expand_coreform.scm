(load "derived_syntax.scm")
(load "constant_folding.scm")
(load "lambda_inlining.scm")
(load "alpha_conversion.scm")
(load "closure_conversion.scm")

(define expand-coreform
  (lambda (form)
  (let* ((form (expand-derived-syntax form))
         (form (alpha-conversion form))
         (form (optimize-constants form))
         (form (lambda-inlining form))
         (form (closure-conversion form)))
`(begin ,@form))))