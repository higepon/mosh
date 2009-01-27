(import (rnrs)
        (mosh)
        (mosh test))

(test* #t #f)
(test* #t #f)
(let loop ([i 0])
  (if (= i 10000)
      '()
      (begin
        (test* #t #t)
        (loop (+ i 1)))))

(test-end)
