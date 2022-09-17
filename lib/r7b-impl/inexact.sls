#!r6rs
(library (r7b-impl inexact)
         (export
acos asin
atan cos
exp finite?
infinite? log
nan? sin
sqrt tan
 )
         (import (rename (rnrs) (finite? r6rs:finite?) (infinite? r6rs:infinite?) (nan? r6rs:nan?))
                 (only (mosh) format))

(define (finite? z)
  (unless (number? z)
    (assertion-violation 'finite? (format #f "~e required, but got ~e" 'number z) `(number ,z)))
  (and (r6rs:finite? (real-part z)) (r6rs:finite? (imag-part z))))

(define (infinite? z)
  (unless (number? z)
    (assertion-violation 'infinite? (format #f "~e required, but got ~e" 'number z) `(number ,z)))
  (or (r6rs:infinite? (real-part z)) (r6rs:infinite? (imag-part z))))

(define (nan? z)
  (unless (number? z)
    (assertion-violation 'nan? (format #f "~e required, but got ~e" 'number z) `(number ,z)))
  (or (r6rs:nan? (real-part z)) (r6rs:nan? (imag-part z))))

)
