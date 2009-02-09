(import (rnrs))

(let loop ([i 0])
  (if (= i 1000000)
      '()
      (begin (+ 1.0 i) (loop (+ i 1)))))
