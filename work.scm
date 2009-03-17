(import (rnrs)
        (mosh test))

(let ([p (standard-output-port)])
      #f
;;       (test* (input-port? p) #f)
;;       (test* (output-port? p) #t)
;;       (test* (binary-port? p) #t)
;      (test* (textual-port? p) #f)
      (close-port p))

(test-end)
