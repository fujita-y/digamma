(cond-expand
  (gauche
   (add-load-path "." :relative)
   (add-load-path "../core" :relative))
  (ypsilon
   (add-load-path "./core")
   (add-load-path "./meta"))
  (else))

(load "repl.scm")

(run-repl)