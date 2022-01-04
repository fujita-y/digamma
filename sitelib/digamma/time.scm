#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (digamma time)
  (export time
          time-usage
          usleep
          microsecond
          microsecond->utc
          microsecond->string
          decode-microsecond
          encode-microsecond)
  (import (core))

  (define-syntax time
    (syntax-rules ()
      ((_ expr)
       (destructuring-bind (real-start user-start sys-start) (time-usage)
         (let ((result (apply (lambda () expr) '())))
           (destructuring-bind (real-end user-end sys-end) (time-usage)
             (format #t
                     "~%;;~10,6f real ~11,6f user ~11,6f sys~%~!"
                     (- real-end real-start)
                     (- user-end user-start)
                     (- sys-end sys-start)))
           result)))))

  ) ;[end]
