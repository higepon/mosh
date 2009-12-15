(import (rnrs))

(define (a) 3)


  (do ([i 0 (+ i 1)])
      [(= i 20)]
    (a))
