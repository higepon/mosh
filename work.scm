(import (rnrs)
        (mosh)
        (mosh trace)
        )
(begin

  (define (tak x y z)
    (if (<= x y)
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y))))

  (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

  (trace tak factorial)
  (tak 5 4 2)
  (factorial 3)
  (untrace tak factorial)
  (tak 5 4 2)
  (factorial 3))
