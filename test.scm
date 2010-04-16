(import (rnrs)
        (rnrs mutable-pairs)
        (srfi :38)
        (mosh test)
        (mosh pp)
        )

(define-syntax test-print
  (lambda (x)
    (syntax-case x ()
      [(_ expected expr write-proc)
       (regexp? (syntax->datum #'expected))
       #'(test-true (expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc)))))]
      [(_ expected expr write-proc)
       #'(test-equal expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc))))])))



(let ([x (cons 'val1 'val2)])
                (set-cdr! x x)
                (write x)
                (newline)
                (write-with-shared-structure x))


(test-equal "#1=(val1 . #1#)" (call-with-values open-string-output-port (lambda (port proc) (write
                                                                                             (let ([x (cons 'val1 'val2)])
                                                                                               (set-cdr! x x)
                                                                                               x)
                                                                                             port) (proc))))


(test-print "#1=(val1 . #1#)" (let ([x (cons 'val1 'val2)])
                                (set-cdr! x x)
                                x) pp)

'#0=(#1=(10 #8# 12)
         #2=(20 21 22)
         #3=(#5# #4# #3#)
         #4=(40 #9# 42)
         #5=(#2# 51 52)
         #6=(60 61 62)
         #7=(#1# 71 #2#)
         #8=(80 81 82)
         #9=(90 91 #7#)
         )
(test-results)
