;; Regression test for https://github.com/higepon/mosh/issues/214.
(define (foo)
  (define (bar  n)
    (cond ((= n 1) #t)
          (else
           (let loop ((lst '(1)))
             (if (null? lst)
                 #t
                 (and 
                  (display "=> recusive1\n")
                  (bar 1)
                  (display "=> loop\n")                         
                  (loop (cdr lst))))))))
    (bar 0)
    )

(foo)