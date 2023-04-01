(library (nmosh io master-queue)
         (export nmosh-io-master-queue)
         (import (rnrs)
                 (nmosh aio platform))
(define nmosh-io-master-queue 
  (begin
    ;(display "====master queue initialized====")(newline)
    (queue)))
)

