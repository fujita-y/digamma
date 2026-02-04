;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Core system loader
;; Loads components in topological order to minimize forward references

;; Common utilities
(load "common.scm")

;; Module
(load "module.scm")

;; Expander components
(load "quasiquote.scm")
(load "syntax_rules.scm")
(load "syntax_case.scm")

;; Main macro expander
(load "macroexpand.scm")

;; Optmizer and Compiler
(load "optimizer.scm")
(load "compiler.scm")
