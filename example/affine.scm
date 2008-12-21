(import (rnrs)
        (mosh string)) ;; format

(define-record-type point (fields x y z w))

(define (point->list p)
  (list (point-x p) (point-y p) (point-z p) (point-w p)))

(define pi 3.14159)

(define-syntax negate
  (syntax-rules ()
    [(_ x)
     (* -1 x)]))

(define (make-translation-matrix tx ty tz)
  `#(1 0 0 ,tx
     0 1 0 ,ty
     0 0 1 ,tz
     0 0 0 1))

(define (make-rotate-x-matrix phi)
  `#(1 0 0 0
     0 ,(cos phi) ,(negate (sin phi)) 0
     0 ,(sin phi) ,(cos phi) 0
     0 0 0 1))

(define (make-rotate-y-matrix phi)
  `#(,(cos phi) ,(negate (sin phi)) 0 0
     ,(sin phi) ,(cos phi) 0 0
     0 0 1 0
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

(define (make-shear-xy-matrix s)
  `#(1 ,s 0  0
     0 1 0  0
     0 0 1  0
     0 0 0  1))

(define (make-matrix a b c)
  (vector (point-x a) (point-y a) (point-y a)
          (point-x b) (point-y b) (point-y b)
          (point-x c) (point-y c) (point-y c)))

(define (print-matrix m)
  (format #t "|~a ~a ~a ~a|\n" (vector-ref m 0)  (vector-ref m 1)  (vector-ref m 2) (vector-ref m 3))
  (format #t "|~a ~a ~a ~a|\n" (vector-ref m 4)  (vector-ref m 5)  (vector-ref m 6) (vector-ref m 7))
  (format #t "|~a ~a ~a ~a|\n" (vector-ref m 8)  (vector-ref m 9)  (vector-ref m 10) (vector-ref m 11))
  (format #t "|~a ~a ~a ~a|\n" (vector-ref m 12) (vector-ref m 13) (vector-ref m 14) (vector-ref m 15)))

(define (mul m p)
  (let ([x (point-x p)]
        [y (point-y p)]
        [z (point-z p)]
        [w (point-w p)])
    (make-point (+ (* x (vector-ref m 0)) (* y (vector-ref m 1)) (* z (vector-ref m 2)) (* w (vector-ref m 3)))
                (+ (* x (vector-ref m 4)) (* y (vector-ref m 5)) (* z (vector-ref m 6)) (* w (vector-ref m 7)))
                (+ (* x (vector-ref m 8)) (* y (vector-ref m 9)) (* z (vector-ref m 10)) (* w (vector-ref m 11)))
                (+ (* x (vector-ref m 12)) (* y (vector-ref m 13)) (* z (vector-ref m 14)) (* w (vector-ref m 15))))))

(define (mulm m1 m2)
  (let-syntax ([ref (syntax-rules ()
                    [(_ v i)
                     (vector-ref v i)])])
  (vector (+ (* (ref m1 0)  (ref m2 0)) (* (ref m1 1)  (ref m2 4)) (* (ref m1 2)  (ref m2 8))  (* (ref m1 3) (ref m2 12)))
          (+ (* (ref m1 0)  (ref m2 1)) (* (ref m1 1)  (ref m2 5)) (* (ref m1 2)  (ref m2 9))  (* (ref m1 3) (ref m2 13)))
          (+ (* (ref m1 0)  (ref m2 2)) (* (ref m1 1)  (ref m2 6)) (* (ref m1 2)  (ref m2 10)) (* (ref m1 3) (ref m2 14)))
          (+ (* (ref m1 0)  (ref m2 3)) (* (ref m1 1)  (ref m2 7)) (* (ref m1 2)  (ref m2 11)) (* (ref m1 3) (ref m2 15)))
          (+ (* (ref m1 4)  (ref m2 0)) (* (ref m1 5)  (ref m2 4)) (* (ref m1 6)  (ref m2 8))  (* (ref m1 7) (ref m2 12)))
          (+ (* (ref m1 4)  (ref m2 1)) (* (ref m1 5)  (ref m2 5)) (* (ref m1 6)  (ref m2 9))  (* (ref m1 7) (ref m2 13)))
          (+ (* (ref m1 4)  (ref m2 2)) (* (ref m1 5)  (ref m2 6)) (* (ref m1 6)  (ref m2 10)) (* (ref m1 7) (ref m2 14)))
          (+ (* (ref m1 4)  (ref m2 3)) (* (ref m1 5)  (ref m2 7)) (* (ref m1 6)  (ref m2 11)) (* (ref m1 7) (ref m2 15)))
          (+ (* (ref m1 8)  (ref m2 0)) (* (ref m1 9)  (ref m2 4)) (* (ref m1 10) (ref m2 8))  (* (ref m1 11) (ref m2 12)))
          (+ (* (ref m1 8)  (ref m2 1)) (* (ref m1 9)  (ref m2 5)) (* (ref m1 10) (ref m2 9))  (* (ref m1 11) (ref m2 13)))
          (+ (* (ref m1 8)  (ref m2 2)) (* (ref m1 9)  (ref m2 6)) (* (ref m1 10) (ref m2 10)) (* (ref m1 11) (ref m2 14)))
          (+ (* (ref m1 8)  (ref m2 3)) (* (ref m1 9)  (ref m2 7)) (* (ref m1 10) (ref m2 11)) (* (ref m1 11) (ref m2 15)))
          (+ (* (ref m1 12) (ref m2 0)) (* (ref m1 13) (ref m2 4)) (* (ref m1 14) (ref m2 8))  (* (ref m1 15) (ref m2 12)))
          (+ (* (ref m1 12) (ref m2 1)) (* (ref m1 13) (ref m2 5)) (* (ref m1 14) (ref m2 9))  (* (ref m1 15) (ref m2 13)))
          (+ (* (ref m1 12) (ref m2 2)) (* (ref m1 13) (ref m2 6)) (* (ref m1 14) (ref m2 10)) (* (ref m1 15) (ref m2 14)))
          (+ (* (ref m1 12) (ref m2 3)) (* (ref m1 13) (ref m2 4)) (* (ref m1 14) (ref m2 11)) (* (ref m1 15) (ref m2 15))))))

(define (transpose m)
  (let-syntax ([ref (syntax-rules ()
                    [(_ v i j)
                     (vector-ref v (+ (* 4 j) i))])])
  (vector (ref m 0 0) (ref m 0 1) (ref m 0 2) (ref m 0 3)
          (ref m 1 0) (ref m 1 1) (ref m 1 2) (ref m 1 3)
          (ref m 2 0) (ref m 2 1) (ref m 2 2) (ref m 2 3)
          (ref m 3 0) (ref m 3 1) (ref m 3 2) (ref m 3 3))))

(define (cross-product a b)
  (make-point (- (* (point-y a) (point-z b)) (* (point-z a) (point-y b)))
              (- (* (point-z a) (point-x b)) (* (point-x a) (point-z b)))
              (- (* (point-x a) (point-y b)) (* (point-y a) (point-x b)))
              1))

(define (orthonormal-axis p)
  (let* ([x (abs (point-x p))]
         [y (abs (point-y p))]
         [z (abs (point-z p))]
         [min (min x y z)])
    (normalize
     (cond
      [(= x min)
       (make-point 0 (negate (point-z p)) (point-y p) 1)]
      [(= y min)
       (make-point (negate (point-z p)) 0 (point-x p) 1)]
      [else
       (make-point (negate (point-y p)) (point-x p) 0 1)]))))

(define (normalize p)
  (let* ([x (abs (point-x p))]
         [y (abs (point-y p))]
         [z (abs (point-z p))]
         [mag (sqrt (+ (* x x) (* y y) (* z z)))])
    (make-point (/ x mag) (/ y mag) (/ z mag) 1)))



;(define before '((1.0 1.0 0.0 1.0) (1.0 2.0 0.0 1.0) (2.0 2.0 0.0 1.0) (2.0 1.0 0.0 1.0)))

(define before '((5.0 0.0 0.0 1.0) (0.0 5.0 0.0 1.0) (0.0 0.0 5.0 1.0)))

;(define t (make-translation-matrix-matrix 1 2 3))

;(define t (make-rotate-z-matrix (/ pi 4)))

;(define t (make-scale-matrix 1 3 1))

;(define t (make-shear-xy-matrix 1.2))

;(define t (mulm (make-rotate-z-matrix (/ pi 4)) (make-translation-matrix 1 2 3)))

;(define t (mulm (make-rotate-z-matrix (/ pi 4)) (make-translation-matrix 1 2 3)))

(define t (mulm (mulm (make-rotate-z-matrix 0) (make-rotate-x-matrix (negate (/ pi 16)))) (make-rotate-y-matrix (negate (/ pi 16)))))

(format #t "(define rect-before '~a)\n" before)
(format #t "(define rect-after '~a)\n"
        (map point->list
             (map (lambda (p) (mul t (apply make-point p))) before)))

(print-matrix t)
(print-matrix (transpose t))

(display (cross-product (make-point 1 0 0 0) (make-point 0 1 0 0)))
(display (normalize (orthonormal-axis (make-point 1 0 0 0))))


;これがおちる。
;(print-matrix mm)

; 3x3 の matrix が欲しい
; printer


(define (make-matrix3 a b c d e f g h i)
  (vector a b c
          d e f
          g h i))

(define (points->matrix3 a b c)
  (make-matrix3 (point-x a) (point-y a) (point-y a)
                (point-x b) (point-y b) (point-y b)
                (point-x c) (point-y c) (point-y c)))

(define (matrix3-ref m i j)
  (vector-ref  m (+ (* 3 j) i)))

(define (print-matrix3 m)
  (format #t "|~a ~a ~a|\n" (matrix3-ref m 0 0) (matrix3-ref m 1 0) (matrix3-ref m 2 0)
  (format #t "|~a ~a ~a|\n" (matrix3-ref m 0 1) (matrix3-ref m 1 1) (matrix3-ref m 2 1)
  (format #t "|~a ~a ~a|\n" (matrix3-ref m 0 2) (matrix3-ref m 1 2) (matrix3-ref m 2 2)))))

(define (transpose-matrix3 m)
  (make-matrix3 (matrix3-ref m 0 0) (matrix3-ref m 0 1) (matrix3-ref m 0 2)
                (matrix3-ref m 1 0) (matrix3-ref m 1 1) (matrix3-ref m 1 2)
                (matrix3-ref m 2 0) (matrix3-ref m 2 1) (matrix3-ref m 2 2)))

(define (make-rotate-x-matrix3 phi)
  (make-matrix3 1 0 0
                0 (cos phi) (negate (sin phi))
                0 (sin phi) (cos phi)))

(define (multiply-matrix3 m1 m2)
  (make-matrix3 (+ (* (matrix3-ref m1 0 0) (matrix3-ref m2 0 0)) (* (matrix3-ref m1 1 0) (matrix3-ref m2 0 1)) (* (matrix3-ref m1 2 0) (matrix3-ref m2 0 2)))
                (+ (* (matrix3-ref m1 0 0) (matrix3-ref m2 1 0)) (* (matrix3-ref m1 1 0) (matrix3-ref m2 1 1)) (* (matrix3-ref m1 2 0) (matrix3-ref m2 1 2)))
                (+ (* (matrix3-ref m1 0 0) (matrix3-ref m2 2 0)) (* (matrix3-ref m1 1 0) (matrix3-ref m2 2 1)) (* (matrix3-ref m1 2 0) (matrix3-ref m2 2 2)))
                (+ (* (matrix3-ref m1 0 1) (matrix3-ref m2 0 0)) (* (matrix3-ref m1 1 1) (matrix3-ref m2 0 1)) (* (matrix3-ref m1 2 1) (matrix3-ref m2 0 2)))
                (+ (* (matrix3-ref m1 0 1) (matrix3-ref m2 1 0)) (* (matrix3-ref m1 1 1) (matrix3-ref m2 1 1)) (* (matrix3-ref m1 2 1) (matrix3-ref m2 1 2)))
                (+ (* (matrix3-ref m1 0 1) (matrix3-ref m2 2 0)) (* (matrix3-ref m1 1 1) (matrix3-ref m2 2 1)) (* (matrix3-ref m1 2 1) (matrix3-ref m2 2 2)))
                (+ (* (matrix3-ref m1 0 2) (matrix3-ref m2 0 0)) (* (matrix3-ref m1 1 2) (matrix3-ref m2 0 1)) (* (matrix3-ref m1 2 2) (matrix3-ref m2 0 2)))
                (+ (* (matrix3-ref m1 0 2) (matrix3-ref m2 1 0)) (* (matrix3-ref m1 1 2) (matrix3-ref m2 1 1)) (* (matrix3-ref m1 2 2) (matrix3-ref m2 1 2)))
                (+ (* (matrix3-ref m1 0 2) (matrix3-ref m2 2 0)) (* (matrix3-ref m1 1 2) (matrix3-ref m2 2 1)) (* (matrix3-ref m1 2 2) (matrix3-ref m2 2 2)))))


;; r (1 1 1 1) を基準に pi/4 回す
(define org (make-point 2 0 0 1))
(define rm (make-point 1 1 1 1))
(define sm (orthonormal-axis rm))
(define tm (cross-product rm sm))

(define mm (points->matrix3 rm sm tm))

(print-matrix3 (multiply-matrix3 (transpose-matrix3 mm) (multiply-matrix3 (make-rotate-x-matrix3 (/ pi 4)) mm)))


;ここで回転。
