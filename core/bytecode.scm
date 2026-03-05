;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;; See LICENSE file for terms and conditions of use.

(define (generate-bytecode all-code)
  (let ((label-map (make-eq-hashtable))
        (final-code '())
        (current-pc 0)
        (const-table (make-equal-hashtable))
        (const-list '())
        (const-pool-start 0)
        (next-const-idx 0))
    ;; Pass 1: Calculate label offsets and code size
    (for-each (lambda (inst)
                (if (eq? (car inst) 'label)
                    (hashtable-set! label-map (cadr inst) current-pc)
                    (set! current-pc (+ current-pc 1))))
              all-code)
    
    (set! const-pool-start current-pc)
    (set! next-const-idx current-pc)

    ;; Pass 2: Generate code and build constant pool
    (for-each (lambda (inst)
                  (if (not (eq? (car inst) 'label))
                      (let ((resolved (map (lambda (x)
                                             (cond ((and (symbol? x) (hashtable-contains? label-map x))
                                                    (hashtable-ref label-map x #f))
                                                   ((and (symbol? x) (let ((s (symbol->string x)))
                                                                       (and (> (string-length s) 1)
                                                                            (char=? (string-ref s 0) #\r)
                                                                            (char-numeric? (string-ref s 1)))))
                                                    (let ((s (symbol->string x)))
                                                      (string->number (substring s 1 (string-length s)))))
                                                   (else x)))
                                           inst)))
                        (if (eq? (car resolved) 'const)
                            (let ((val (caddr resolved)))
                              (if (hashtable-contains? const-table val)
                                  (set-car! (cddr resolved) (hashtable-ref const-table val #f))
                                  (begin
                                    (hashtable-set! const-table val next-const-idx)
                                    (set-car! (cddr resolved) next-const-idx)
                                    (set! const-list (cons val const-list))
                                    (set! next-const-idx (+ next-const-idx 1))))))
                        (set! final-code (cons (list->vector resolved) final-code)))))
              all-code)
    
    (list->vector (append (reverse final-code) (reverse const-list)))))
