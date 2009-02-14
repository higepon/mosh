#!r6rs
; based on http://lucille.atso-net.jp/aobench/
;          http://d.hatena.ne.jp/mjt/20090209/p1
(import (rnrs) (srfi :8) (srfi :27))

(define PI 3.141592)

(define-syntax vec
  (lambda (v)
    (syntax-case v ()
      [(_ x y z) #'(vector x y z)])))

(define-syntax vx
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'(vector-ref v 0)])))

(define-syntax vy
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'(vector-ref v 1)])))

(define-syntax vz
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'(vector-ref v 2)])))

(define-syntax square
  (lambda (x)
    (syntax-case x ()
      [(_ v) #'(* v v)])))

(define (vadd a b)
  (vec (+ (vx a) (vx b)) (+ (vy a) (vy b)) (+ (vz a) (vz b))))
(define (vsub a b)
  (vec (- (vx a) (vx b)) (- (vy a) (vy b)) (- (vz a) (vz b))))
(define (vcross a b)
  (vec
    (- (* (vy a) (vz b)) (* (vy b) (vz a)))
    (- (* (vz a) (vx b)) (* (vz b) (vx a)))
    (- (* (vx a) (vy b)) (* (vx b) (vy a)))))
(define (vdot a b)
  (+ (* (vx a) (vx b)) (* (vy a) (vy b)) (* (vz a) (vz b))))
(define (vlen a)
  (flsqrt (+ (square (vx a)) (square (vy a)) (square (vz a)))))


(define (vnormalize a)
  (let ((len (vlen a)))
    (if (> (abs len) 1.0e-6)
      (let ((invlen (/ 1.0 len)))
        (vec 
          (* (vx a) invlen)
          (* (vy a) invlen)
          (* (vz a) invlen))))))


;R6RS record version
(define-record-type 
  (intersection make-intersection intersection?)
  (fields
    (immutable t is-t)
    (immutable p is-p)
    (immutable n is-n)
    (immutable hit is-hit)))

(define (Sphere center radius)
  (lambda (isect2 ray)
    (let* ((rs (vsub (org ray) center))
           (B (vdot rs (dir ray)))
           (C (- (vdot rs rs) (square radius)))
           (D (- (square B) C)))
      (if (> D 0.0) ; update isect2
        (let ((t (- 0.0 B (sqrt D))))
          (if (< 0.0 t (is-t isect2))
            (let ((p (vec 
                       (+ (vx (org ray)) (* (vx (dir ray)) t))
                       (+ (vy (org ray)) (* (vy (dir ray)) t))
                       (+ (vz (org ray)) (* (vz (dir ray)) t)))))
            (make-intersection
              t
              p
              (vnormalize (vsub p center))
              #t)) isect2)) 
        isect2))))



(define (Plane p n)
  (lambda (isect ray)
    (let ((d (- 0.0 (vdot p n)))
          (v (vdot (dir ray) n)))
      (if (> (abs v) 1.0e-6) ; update isect
        (let ((t (/ (- 0.0 (+ (vdot (org ray) n) d)) v)))
          (if (< 0.0 t (is-t isect))
            (make-intersection
              t
              (vec
                (+ (vx (org ray)) (* (vx (dir ray)) t))
                (+ (vy (org ray)) (* (vy (dir ray)) t))
                (+ (vz (org ray)) (* (vz (dir ray)) t)))
              n
              #t) isect)) 
        isect))))

(define (vneg a)
  (vec (- 0.0 (vx a)) (- 0.0 (vy a)) (- 0.0 (vz a))))

(define org car)
(define dir cdr)
(define new-ray cons)

(define IMAGE_WIDTH 256)
(define IMAGE_HEIGHT 1) ;; 256 for bench
(define NSUBSAMPLES 2)
(define NAO_SAMPLES 8)

(define (main)
  (let ((myport (open-file-output-port "out.rgb")))
  (define (line-loop y)
    (cond
      ((= y IMAGE_HEIGHT)
       (display "DONE")(newline))
      (else
        (let ((w (render IMAGE_WIDTH IMAGE_HEIGHT y NSUBSAMPLES)))
          (display "LINE")(display y)(display " - ")(display (bytevector-length w))(newline)
          (put-bytevector myport w))
        (line-loop (+ y 1)))))
  (line-loop 0)(close-port myport)))


(define theS0 (Sphere (vec -2.0 0.0 -3.5) 0.5))
(define theS1 (Sphere (vec -0.5 0.0 -3.0) 0.5))
(define theS2 (Sphere (vec 1.0 0.0 -2.2) 0.5))
(define thePlane (Plane (vec 0.0 -0.5 0.0) (vec 0.0 1.0 0.0)))

(define (clamp f)
  (let ((i (exact (floor (* f 255.5)))))
    (cond
      ((< i 0) 0)
      ((> i 255) 255)
      (else i))))

(define (orthoBasis n)
  (let* ((v
          (cond 
            ((< -0.6 (vx n) 0.6)
             (vec 1.0 0.0 0.0))
            ((< -0.6 (vy n) 0.6)
             (vec 0.0 1.0 0.0))
            ((< -0.6 (vz n) 0.6)
             (vec 0.0 0.0 1.0))
            (else (vec 1.0 0.0 0.0))))
        (s (vnormalize (vcross v n))))
    (values
      s
      (vnormalize (vcross n s))
      n)))

(define (ambientOcclusion isect)
  (let* 
    ((eps 0.00001)
     (ntheta NAO_SAMPLES)
     (nphi NAO_SAMPLES)
     (p (vec (+ (vx (is-p isect)) (* eps (vx (is-n isect))))
             (+ (vy (is-p isect)) (* eps (vy (is-n isect))))
             (+ (vz (is-p isect)) (* eps (vz (is-n isect)))))))
    (define (ntheta-nphi-loop j i occlusion)
      (cond 
        ((= j nphi) (/ (- (* ntheta nphi) occlusion) (* ntheta nphi)))
        ((= i nphi) (ntheta-nphi-loop (+ j 1) 0 occlusion))
        (else
          (let* ((r 1.0)
                 (phi (* 2.0 PI 1.0))
                 (x (* (cos phi) (sqrt (- 1.0 r))))
                 (y (* (sin phi) (sqrt (- 1.0 r))))
                 (z (sqrt r)))
            (receive (b0 b1 b2) (orthoBasis (is-n isect))
                     (let ((ray (new-ray p (vec
                                             ;; (+ (* x (vx b0)) (* y (vx b1)) (* z (vx b2)))
;;                                              (+ (* x (vy b0)) (* y (vy b1)) (* z (vy b2)))
;;                                              (+ (* x (vz b0)) (* y (vz b1)) (* z (vz b2)))
                                            1.0 1.0 1.0
                                             ))))
                       (ntheta-nphi-loop j (+ 1 i) 
                                  (if (is-hit (thePlane (theS2 (theS1 (theS0 
                                                                        (make-intersection 1.0e+30 
                                                                                           (vec 0.0 0.0 0.0) 
                                                                                           (vec 0.0 0.0 0.0) 
                                                                                           #f) ray) ray) ray) ray))
                                    (+ occlusion 1.0)
                                    occlusion)))))))) (ntheta-nphi-loop 0 0 0.0)))

(define (render width height y nsubsamples)
  (let ((img (make-bytevector (* width 4))))
    (define (render-sample-loop i u v sum)
      (cond
        ((= v nsubsamples) sum)
        ((= u nsubsamples) (render-sample-loop i 0 (+ v 1) sum))
        (else
          (let ((px (/ (+ i (- (/ u nsubsamples) (/ width 2.0))) (/ width 2.0)))
                (py (- 0.0 (/ (+ y (- (/ v nsubsamples) (/ height 2.0))) (/ height 2.0)))))
            (let* ((ray (new-ray (vec 0.0 0.0 0.0) (vnormalize (vec px py -1.0))))
                   (isect
                     (thePlane (theS2 (theS1 (theS0 
                                               (make-intersection 1.0e+30 
                                                                  (vec 0.0 0.0 0.0) 
                                                                  (vec 0.0 0.0 0.0) 
                                                                  #f) ray) ray) ray) ray)))
              (render-sample-loop i (+ u 1) v 
                                  (if (is-hit isect)
                                    (+ sum (ambientOcclusion isect))
                                    sum)))))))

    (define (render-pixel-loop i)
      (if (= i width)
        img
        (let ((sum (render-sample-loop i 0 0 0.0)))
                   (bytevector-u8-set! img (+ (* i 4) 0) (clamp (/ sum (square nsubsamples))))
                   (bytevector-u8-set! img (+ (* i 4) 1) (clamp (/ sum (square nsubsamples))))
                   (bytevector-u8-set! img (+ (* i 4) 2) (clamp (/ sum (square nsubsamples))))
          (render-pixel-loop (+ i 1))))) (render-pixel-loop 0)))

(main)
