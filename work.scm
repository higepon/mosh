(import (rnrs)
        (srfi :26)
        )

;; (for-each 
;;  (lambda (x) (display x))
;;           '(1 2 32))


(let ([foo (case-lambda 
            (() 'zero)
            ((x) (list 'one x)))])
  (display (foo))
  (display (foo 'x)))
           
