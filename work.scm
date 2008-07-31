(let1 p (test) ;(test) ;(test);test2
  (let loop ([i 0])
    (if (> i 10000000)
        i
        (begin (p)
        (loop (+ i 1))))))
