(import (rnrs)
        (mosh))

(let loop ([i 0])
  (if (= i 100000)
      (display "\ndone")
      (begin
        (format #t "~d\r" i)
        (loop (+ i 1)))))
