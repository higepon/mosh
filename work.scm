(import (rnrs)
        (srfi :64))

<<<<<<< .mine
(define-syntax :bit
  (lambda (x)
    (syntax-case x ()
      [(_ ([a m] [b n]) v body ...)
       (and (fixnum? (syntax->datum #'m)) (fixnum? (syntax->datum #'n))
            (zero? (mod (+ (syntax->datum #'m) (syntax->datum #'n)) 8)))
       #'(let ([bits (+ n m)])
           (let ([a (bitwise-arithmetic-shift-right v (- bits m))]
                 [b (bitwise-and v (- (expt 2 n) 1))])
             body ...))])))

(test-begin "Example of the bit syntax")

;; unpack
(:bit ([x 2] [y 6]) #b10101110
  (test-eqv 2 x)
  (test-eqv 46 y))

(test-end)
=======
(define (foo)
  (foo))

(foo)
>>>>>>> .r1562
