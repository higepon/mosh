(import (rnrs))

(define-record-type a
  (fields
   (mutable value)))
(define ar (make-a 3))
(let loop ([i 0])
  (cond
   [(= i 1000000)
    (display "done\n")]
   [else
    (make-a 3)
    (loop (+ i 1))]))
