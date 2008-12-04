(define a 1)
(display
 (let loop ([i 0])
   (if (= i 10)
       'done
       (let1 b a
         (loop (+ i b))))))

