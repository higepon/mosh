(import (mosh)
        (rnrs)
        (only (mosh pp) pp))

(define-syntax y
  (lambda (x)
    (syntax-case x ()
      ([_ z]
       (and (pp #'z))
       #'d))))
(let ([x 1])
(y x))
