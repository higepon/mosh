(import (rnrs)
#;        (mosh jit compiler))

(define (a) 3)
;(display compile)
  (do ([i 0 (+ i 1)])
      [(= i 200000)]
    (a))
