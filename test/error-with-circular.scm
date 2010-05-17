(import (rnrs)
        (rnrs mutable-pairs))

(let ([x (cons 'a 'a)])
  (set-car! x x)
  ;; this causes error.
  ;; Mosh shows the irritant 'x' which is circular.
  (string-append x))
