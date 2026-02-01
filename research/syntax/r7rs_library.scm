;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.
;;
;; R7RS Library System - Portable Implementation
;;
;; This module implements R7RS-style library definitions and imports using
;; syntax-case macros. Libraries are stored in a global registry at runtime.
;;
;; Public macros:
;;   (r7rs:define-library (name ...) decl ...)  - Define a library
;;   (r7rs:import import-set ...)               - Import bindings
;;
;; Supported declarations in define-library:
;;   (export id ...)        - Export identifiers
;;   (import import-set...) - Import from other libraries
;;   (begin form ...)       - Library body
;;
;; Supported import modifiers:
;;   (only import-set id ...)         - Keep only specified identifiers
;;   (except import-set id ...)       - Remove specified identifiers
;;   (rename import-set (old new)...) - Rename identifiers
;;   (prefix import-set pfx)          - Add prefix to all identifiers

;;=============================================================================
;; 1. Global Library Registry
;;=============================================================================

;; Registry mapping library names to records: ((name . record) ...)
;; Each record is (exports . bindings) where:
;;   exports:  list of exported identifier symbols
;;   bindings: alist of (identifier . value) pairs
(define r7rs:*libraries* '())

;; Temporary variable for passing values through eval
(define r7rs:*temp-value* #f)

;; Register a library in the global registry
(define (r7rs:register-library! name exports bindings)
  (set! r7rs:*libraries* 
        (cons (cons name (cons exports bindings)) 
              r7rs:*libraries*)))

;; Lookup a library by name, returns (exports . bindings) or #f
(define (r7rs:lookup-library name)
  (let ((entry (assoc name r7rs:*libraries*)))
    (and entry (cdr entry))))

;;=============================================================================
;; 2. Declaration Scanners
;;=============================================================================

;; Generic scanner: extract and flatten contents of declarations by keyword
;; Returns concatenated (cdr ...) of all declarations matching the keyword
(define (r7rs:scan-decls keyword decls)
  (let loop ((decls decls) (result '()))
    (cond
      ((null? decls) (reverse result))
      ((and (pair? (car decls)) (eq? (caar decls) keyword))
       (loop (cdr decls) (append (reverse (cdar decls)) result)))
      (else
       (loop (cdr decls) result)))))

;; Convenience wrappers for specific declaration types
(define (r7rs:scan-exports decls) (r7rs:scan-decls 'export decls))
(define (r7rs:scan-imports decls) (r7rs:scan-decls 'import decls))
(define (r7rs:scan-begins decls)  (r7rs:scan-decls 'begin decls))

;;=============================================================================
;; 3. Import Set Processing
;;=============================================================================

;; Extract the base library name from an import set by unwrapping modifiers
(define (r7rs:extract-library-name import-set)
  (if (and (pair? import-set)
           (memq (car import-set) '(only except rename prefix)))
      (r7rs:extract-library-name (cadr import-set))
      import-set))

;; Apply import modifiers to transform bindings
;; spec: import specification (library name or modifier form)
;; bindings: alist of (id . value) to transform
;; Returns transformed bindings alist
(define (r7rs:apply-import-spec spec bindings)
  (if (not (pair? spec))
      bindings
      (case (car spec)
        ((only)
         (let ((ids (cddr spec))
               (inner (r7rs:apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (memq (car b) ids)) inner)))
        
        ((except)
         (let ((ids (cddr spec))
               (inner (r7rs:apply-import-spec (cadr spec) bindings)))
           (filter (lambda (b) (not (memq (car b) ids))) inner)))
        
        ((rename)
         (let ((renames (cddr spec))
               (inner (r7rs:apply-import-spec (cadr spec) bindings)))
           (map (lambda (b)
                  (let ((rename (assq (car b) renames)))
                    (if rename
                        (cons (cadr rename) (cdr b))
                        b)))
                inner)))
        
        ((prefix)
         (let ((pfx (caddr spec))
               (inner (r7rs:apply-import-spec (cadr spec) bindings)))
           (map (lambda (b)
                  (cons (string->symbol
                         (string-append (symbol->string pfx)
                                        (symbol->string (car b))))
                        (cdr b)))
                inner)))
        
        ;; Unrecognized form - treat as plain library name
        (else bindings))))

;; Process an import set: lookup library and apply modifiers
;; Returns filtered and transformed bindings alist
(define (r7rs:process-import-set import-set)
  (let* ((lib-name (r7rs:extract-library-name import-set))
         (record (r7rs:lookup-library lib-name)))
    (unless record
      (error "Library not found" lib-name))
    (let* ((exports (car record))
           (bindings (cdr record))
           (exported (filter (lambda (b) (memq (car b) exports)) bindings)))
      (r7rs:apply-import-spec import-set exported))))

;;=============================================================================
;; 4. Definition Extraction
;;=============================================================================

;; Extract defined identifiers from body forms
;; Handles: (define var ...) and (define (func args...) ...)
(define (r7rs:extract-defined-ids forms)
  (let loop ((forms forms) (ids '()))
    (if (null? forms)
        (reverse ids)
        (let ((form (car forms)))
          (loop (cdr forms)
                (if (and (pair? form) (eq? (car form) 'define))
                    (let ((pattern (cadr form)))
                      (cons (if (pair? pattern) (car pattern) pattern) ids))
                    ids))))))

;;=============================================================================
;; 5. Library Body Evaluation
;;=============================================================================

;; Inject a binding into the interaction environment
(define (r7rs:inject-binding! name value)
  (set! r7rs:*temp-value* value)
  (eval `(define ,name r7rs:*temp-value*) (interaction-environment)))

;; Evaluate library body in an isolated lexical scope using a lambda wrapper.
;; Imports are quoted into defines; body forms are spliced in; defined values
;; are collected and returned as an alist. This prevents internal bindings
;; from leaking to the global environment.
(define (r7rs:eval-library-body imported-bindings body-forms)
  (let* ((defined-ids (r7rs:extract-defined-ids body-forms))
         ;; Quote imported values into define forms
         (import-defs (map (lambda (b) `(define ,(car b) ',(cdr b))) 
                          imported-bindings))
         ;; Lambda wrapper: defines + body + return list of defined values
         (wrapper `(lambda ()
                     ,@import-defs
                     ,@body-forms
                     (list ,@defined-ids))))
    ;; Compile wrapper, invoke it, and pair ids with values
    (let* ((proc (eval wrapper (interaction-environment)))
           (vals (proc)))
      (map cons defined-ids vals))))

;;=============================================================================
;; 6. r7rs:define-library Macro
;;=============================================================================

;; (r7rs:define-library (name ...) decl ...)
;; Define a library. Declarations can be export, import, or begin forms.
(define-syntax r7rs:define-library
  (lambda (x)
    (syntax-case x ()
      ((_ (lib-name ...) decl ...)
       (syntax
        (r7rs:define-library-helper (quote (lib-name ...)) 
                                    (quote (decl ...))))))))

;; Runtime helper: process declarations and register the library
(define (r7rs:define-library-helper lib-name decls)
  (let* ((exports (r7rs:scan-exports decls))
         (imports (r7rs:scan-imports decls))
         (body-forms (r7rs:scan-begins decls))
         (imported-bindings (apply append (map r7rs:process-import-set imports)))
         (result-bindings (r7rs:eval-library-body imported-bindings body-forms)))
    ;; Register the library with its exports and all defined bindings
    (r7rs:register-library! lib-name exports result-bindings)
    'defined))

;;=============================================================================
;; 7. r7rs:import Macro
;;=============================================================================

;; (r7rs:import import-set ...)
;; Import bindings from libraries into the current environment
(define-syntax r7rs:import
  (lambda (x)
    (syntax-case x ()
      ((_ import-set ...)
       (syntax
        (r7rs:import-helper (quote (import-set ...))))))))

;; Runtime helper: process imports and inject bindings
(define (r7rs:import-helper import-sets)
  (let ((all-bindings (apply append (map r7rs:process-import-set import-sets))))
    (for-each (lambda (b) (r7rs:inject-binding! (car b) (cdr b)))
              all-bindings)
    'imported))
