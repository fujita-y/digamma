(cond-expand
  (gauche
   (use srfi-9)
   (add-load-path "." :relative)
   (add-load-path "../core" :relative))
  (ypsilon
   (import (srfi 9))
   (add-load-path "./core")
   (add-load-path "./meta"))
  (else))

(load "repl.scm")

(run-repl)