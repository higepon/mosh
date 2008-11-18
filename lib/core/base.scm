(define (mod x y)
  (- x (* (div x y) y)))

(define (div-and-mod x y)
  (let ([d (div x y)])
      (values d (- x (* d y)))))

(define (mod0 x y)
    (- x (* (div0 x y) y)))

(define (div0-and-mod0 x y)
    (let ([d0 (div0 x y)])
      (values d0 (- x (* d0 y)))))
