(import (rnrs)
        (srfi :8))

(display (receive (a b . c) (values 1 2 3 4 5)
         (apply + a b c)))
(display "hige")


