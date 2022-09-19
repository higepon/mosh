#!r6rs
(library (r7b-util time)
         (export current-jiffy current-second jiffies-per-second)
         (import (rnrs)
                 (srfi i19))

(define scale 1000000000)

(define (jiffies-per-second) scale)
(define (current-jiffy) (return-sec time-monotonic scale))
(define (current-second) (return-sec time-tai (inexact scale)))

(define (return-sec sym scale)
  (let ((t (current-time sym)))
    (+ (* scale (time-nanosecond t))
       (time-second t))))

)
