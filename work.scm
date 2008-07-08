(define a 3)
(define b 4)
(define c 5)
(let1 x a
  (let1 x b
    (let1 z 5
      (print (+ x z)))))
