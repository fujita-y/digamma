;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

;; Core system loader
;; Loads components in topological order to minimize forward references

;; Compatibility
(load "host.scm")

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
(load "bytecode.scm")
(load "compiler.scm")
