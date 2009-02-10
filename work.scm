(let loop ([i 0])
  (if (= i 10000000)
      '()
      (loop (+ i 1))))
