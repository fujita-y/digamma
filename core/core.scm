;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Core system loader
;; Loads components in topological order to minimize forward references

;; Common utilities
(load "common.scm")

;; Module
(load "env.scm")

;; Expander components
(load "quasiquote.scm")
(load "syntax-rules.scm")
(load "syntax-case.scm")

;; Main macro expander
(load "macroexpand.scm")

;; Optmizer and Compiler
(load "optimizer.scm")
(load "compiler.scm")
