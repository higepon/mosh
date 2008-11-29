(import (rnrs)
        (mosh string))

(define-record-type point (fields x y z w))

(define (make-transform-matrix tx ty tz)
  `#(1 0 0 ,tx
     0 1 0 ,ty
     0 0 1 ,tz
     0 0 0 1))

(define (mul p m)
  (let ([x (point-x p)]
        [y (point-y p)]
        [z (point-z p)]
        [w (point-w p)])
    (make-point (+ (* x (vector-ref m 0)) (* x (vector-ref m 1)) (* x (vector-ref m 2)) (* x (vector-ref m 3)))
                (+ (* y (vector-ref m 4)) (* y (vector-ref m 5)) (* y (vector-ref m 6)) (* y (vector-ref m 7)))
                (+ (* z (vector-ref m 8)) (* z (vector-ref m 9)) (* z (vector-ref m 10)) (* z (vector-ref m 11)))
                (+ (* w (vector-ref m 12)) (* w (vector-ref m 13)) (* w (vector-ref m 14)) (* w (vector-ref m 15))))))

(let ([org-point (make-point 1 2 3 1)])
  (format #t "simple-transform: org-point ~a => ~a\n" org-point (mul org-point (make-transform-matrix 5 0 0))))
