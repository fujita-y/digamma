(import (digamma gl) (digamma glut) (digamma c-types))
(glutInit (make-c-int (c-main-argc)) (c-main-argv))
(glutCreateWindow (string->utf8/nul "Digamma"))
(format #t "~a~%~!" (utf8->string (make-bytevector-mapping (glGetString GL_VERSION) 1024)))
