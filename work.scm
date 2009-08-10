(import (rnrs))

(define-syntax let1
    (lambda (x)
      (syntax-case x ()
        [(_ var val body body* ...)
         #'(let ([var val]) body body* ...)])))

(display (let1 x 3 x x))
