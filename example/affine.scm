(import (rnrs)
        (mosh string)) ;; format

(define-record-type point (fields x y z w))

(define pi 3.14159)

(define-syntax negate
  (syntax-rules ()
    [(_ x)
     (* -1 x)]))

(define (make-transform-matrix tx ty tz)
  `#(1 0 0 ,tx
     0 1 0 ,ty
     0 0 1 ,tz
     0 0 0 1))

(define (make-rotate-z-matrix phi)
  `#(,(cos phi) ,(negate (sin phi)) 0 0
     ,(sin phi) ,(cos phi) 0 0
     0 0 1 0
     0 0 0 1))

(define (make-scale-matrix sx sy sz)
  `#(,sx 0   0   0
     0   ,sy 0   0
     0   0   ,sz 0
     0   0   0   1))

(define (make-shear-xz-matrix s)
  `#(1 0 ,s 0
     0 1 0  0
     0 0 1  0
     0 0 0  1))


(define (mul m p)
  (let ([x (point-x p)]
        [y (point-y p)]
        [z (point-z p)]
        [w (point-w p)])
    (make-point (+ (* x (vector-ref m 0)) (* y (vector-ref m 1)) (* z (vector-ref m 2)) (* w (vector-ref m 3)))
                (+ (* x (vector-ref m 4)) (* y (vector-ref m 5)) (* z (vector-ref m 6)) (* w (vector-ref m 7)))
                (+ (* x (vector-ref m 8)) (* y (vector-ref m 9)) (* z (vector-ref m 10)) (* w (vector-ref m 11)))
                (+ (* x (vector-ref m 12)) (* y (vector-ref m 13)) (* z (vector-ref m 14)) (* w (vector-ref m 15))))))

(let ([org-point (make-point 1 2 3 1)])
  (format #t "simple-transformation: org-point ~a => ~a\n" org-point (mul (make-transform-matrix 5 0 0) org-point ))
  (format #t "pi/2 rotation around z axis: org-point ~a => ~a\n" org-point (mul (make-rotate-z-matrix (/ pi 2)) org-point ))
  (format #t "scaling: org-point ~a => ~a\n" org-point (mul (make-scale-matrix 1 1 3) org-point ))
  (format #t "shearing: org-point ~a => ~a\n" org-point (mul (make-shear-xz-matrix 2) org-point ))
  (format #t "concatenation of rotation & scaling ~a => ~a\n" org-point (mul (make-scale-matrix 3 1 1) (mul (make-rotate-z-matrix (/ pi 2)) org-point)))
  (format #t "concatenation of scaling & rotation ~a => ~a\n" org-point (mul (make-rotate-z-matrix (/ pi 2)) (mul (make-scale-matrix 3 1 1) org-point)))
)
