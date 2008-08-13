(let loop ([i 0])
  (let1 x (lambda (a b c) a b c)
  (if (> i 500000)
      i
      (begin
        (apply x '(a b c))
        (loop (+ i 1))))))
