(import (rnrs))
(define (fact n)
             (let f ((n n) (r 1))
               (if (< n 2) r

                  (f (- n 1) (* r n)))))
(fact 100000)
