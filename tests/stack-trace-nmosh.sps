(import (nmosh debugger) ;; trigger debugger over-ride for test
        (rnrs))
(define (a)
  (display 3 3 3)
  4
  )

(define (b)
  (a)
  3
  )

(b)
