(define fits?
  (lambda (w.1 lst.2)
    (if (>= w.1 0)
        (let ((tmp.3.4 (null? lst.2)))
          (if tmp.3.4
              tmp.3.4
              (let ((tmp_69bb85886cd2213.172 lst.2) (tmp_69bb85886ceb614.173 string?))
                (let ((tmp_69bb85886d0e315.174
                        (if (pair? tmp_69bb85886cd2213.172)
                            (if (pair? (car tmp_69bb85886cd2213.172))
                                (if (pair? (cdar tmp_69bb85886cd2213.172))
                                    (pair? (cddar tmp_69bb85886cd2213.172))
                                    #f)
                                #f)
                            #f)))
                  (let ((tmp_69bb85886d18916.175
                          (if tmp_69bb85886d0e315.174 (null? (cdddar tmp_69bb85886cd2213.172)) #f)))
                    (let ((tmp_69bb85886d26b17.176
                            (if tmp_69bb85886d0e315.174
                                (pair? (caddar tmp_69bb85886cd2213.172))
                                #f)))
                      (let ((tmp_69bb85886d34318.177
                              (if tmp_69bb85886d26b17.176
                                  (null? (cdddar tmp_69bb85886cd2213.172))
                                  #f)))
                        (if (if tmp_69bb85886d0e315.174
                                (if (null? (caddar tmp_69bb85886cd2213.172))
                                    (null? (cdddar tmp_69bb85886cd2213.172))
                                    #f)
                                #f)
                            (let ((z.178 (cdr tmp_69bb85886cd2213.172))) (fits? w.1 z.178))
                            (if (if tmp_69bb85886d18916.175
                                    (if (eq? (cadar tmp_69bb85886cd2213.172) '|.&BREAK|)
                                        (eq? (caddar tmp_69bb85886cd2213.172) #\;)
                                        #f)
                                    #f)
                                (let ((z.179 (cdr tmp_69bb85886cd2213.172))) #t)
                                (if (if tmp_69bb85886d18916.175
                                        (if (eq? (cadar tmp_69bb85886cd2213.172) '|.&FLAT|)
                                            (eq? (caddar tmp_69bb85886cd2213.172) #\;)
                                            #f)
                                        #f)
                                    (let ((z.180 (cdr tmp_69bb85886cd2213.172)))
                                      (fits? (- w.1 1) z.180))
                                    (if (if tmp_69bb85886d18916.175
                                            (tmp_69bb85886ceb614.173
                                              (caddar tmp_69bb85886cd2213.172))
                                            #f)
                                        (let ((z.181 (cdr tmp_69bb85886cd2213.172))
                                              (s.182 (caddar tmp_69bb85886cd2213.172)))
                                          (fits? (- w.1 (string-length s.182)) z.181))
                                        (if (if tmp_69bb85886d34318.177
                                                (eq?
                                                  (car (caddar tmp_69bb85886cd2213.172))
                                                  '|.&GROUP|)
                                                #f)
                                            (let ((z.183 (cdr tmp_69bb85886cd2213.172))
                                                  (x.184 (cdr (caddar tmp_69bb85886cd2213.172)))
                                                  (i.185 (caar tmp_69bb85886cd2213.172)))
                                              (fits? w.1 (cons (list i.185 '|.&FLAT| x.184) z.183)))
                                            (if (if tmp_69bb85886d26b17.176
                                                    (if (pair?
                                                          (cdr (caddar tmp_69bb85886cd2213.172)))
                                                        (if (null? (cdddar tmp_69bb85886cd2213.172))
                                                            (eq?
                                                              (car (caddar tmp_69bb85886cd2213.172))
                                                              '|.&NEST|)
                                                            #f)
                                                        #f)
                                                    #f)
                                                (let ((z.186 (cdr tmp_69bb85886cd2213.172))
                                                      (x.187
                                                        (cddr (caddar tmp_69bb85886cd2213.172)))
                                                      (j.188
                                                        (cadr (caddar tmp_69bb85886cd2213.172)))
                                                      (m.189 (cadar tmp_69bb85886cd2213.172))
                                                      (i.190 (caar tmp_69bb85886cd2213.172)))
                                                  (fits?
                                                    w.1
                                                    (cons (list (+ i.190 j.188) m.189 x.187)
                                                          z.186)))
                                                (if tmp_69bb85886d34318.177
                                                    (let ((z.191 (cdr tmp_69bb85886cd2213.172))
                                                          (y.192
                                                            (cdr (caddar tmp_69bb85886cd2213.172)))
                                                          (x.193
                                                            (car (caddar tmp_69bb85886cd2213.172)))
                                                          (m.194 (cadar tmp_69bb85886cd2213.172))
                                                          (i.195 (caar tmp_69bb85886cd2213.172)))
                                                      (fits?
                                                        w.1
                                                        (cons (list i.195 m.194 x.193)
                                                              (cons (list i.195 m.194 y.192)
                                                                    z.191))))
                                                    #f))))))))))))))
        #f)))
