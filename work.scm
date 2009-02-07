#!r6rs
; based on http://lucille.atso-net.jp/rwp/pt/
(import (rnrs) (srfi :8) (srfi :27))

(define PI 3.141592)

(define (vec x y z)
  (let ((v (make-bytevector 24)))
    (bytevector-ieee-double-native-set! v 0 x)
    (bytevector-ieee-double-native-set! v 8 y)
    (bytevector-ieee-double-native-set! v 16 z) v))
(define (vx x)
  (bytevector-ieee-double-native-ref x 0))
(define (vy x)
  (bytevector-ieee-double-native-ref x 8))
(define (vz x)
  (bytevector-ieee-double-native-ref x 16))
(define (vadd a b)
  (vec (+ (vx a) (vx b)) (+ (vy a) (vy b)) (+ (vz a) (vz b))))
(define (vsub a b)
  (vec (- (vx a) (vx b)) (- (vy a) (vy b)) (- (vz a) (vz b))))
(define (vcross a b)
  (vec
    (- (* (vy a) (vz b)) (* (vy b) (vz a)))
    (- (* (vz a) (vx b)) (* (vz b) (vx a)))
    (- (* (vx a) (vy b)) (* (vx b) (vy a)))))
(define (vnormalize a)
  (let ((len (vlen a)))
    (if (> (abs len) 1.0e-6)
      (let ((invlen (/ 1.0 len)))
	(vec 
	  (* (vx a) invlen)
	  (* (vy a) invlen)
	  (* (vz a) invlen))))))
(define (square a)
  (* a a))
(define (vlen a)
  (sqrt (+ (square (vx a)) (square (vy a)) (square (vz a)))))
(define (vdot a b)
  (+ (* (vx a) (vx b)) (* (vy a) (vy b)) (* (vz a) (vz b))))
(define (vneg a)
  (vec (- 0.0 (vx a)) (- 0.0 (vy a)) (- 0.0 (vz a))))
(define (vscale v x)
  (vec (* x (vx v)) (* x (vy v)) (* x (vz v))))

(define org car)
(define dir cdr)
(define new-ray cons)

;R6RS record version
(define-record-type 
  (intersection make-intersection intersection?)
  (fields
    (mutable t is-t is-t-set!)
    (mutable p is-p is-p-set!)
    (mutable n is-n is-n-set!)
    (mutable col is-col is-col-set!)
    (mutable emissiveCol is-emissiveCol is-emissiveCol-set!)
    (mutable hit is-hit is-hit-set!))
  (protocol
    (lambda (c)
      (lambda ()
	(c 1.0e+30 (vec 0.0 0.0 0.0) (vec 0.0 0.0 0.0) (vec 0.0 0.0 0.0) (vec 0.0 0.0 0.0) #f)))))

(define (Sphere center radius col emissiveCol)
  (lambda (isect2 ray)
    (let* ((rs (vsub (org ray) center))
	   (B (vdot rs (dir ray)))
	   (C (- (vdot rs rs) (square radius)))
	   (D (- (square B) C)))
      (if (> D 0.0)
	(let ((t (- 0.0 B (sqrt D))))
	  (if (< 0.0 t (is-t isect2))
	    (begin
	    (is-t-set! isect2 t)
	    (is-hit-set! isect2 #t)
	    (let ((p (vec 
		       (+ (vx (org ray)) (* (vx (dir ray)) t))
		       (+ (vy (org ray)) (* (vy (dir ray)) t))
		       (+ (vz (org ray)) (* (vz (dir ray)) t)))))
	      (is-n-set! isect2 (vnormalize (vsub p center)))
	      (is-p-set! isect2 p))
	    (is-col-set! isect2 col)
	    (is-emissiveCol-set! isect2 emissiveCol))))))))

(define (Plane p n col emissiveCol)
  (lambda (isect ray)
    (let ((d (- 0.0 (vdot p n)))
	  (v (vdot (dir ray) n)))
      (if (> (abs v) 1.0e-6)
	(let ((t (/ (- 0.0 (+ (vdot (org ray) n) d)) v)))
	  (if (< 0.0 t (is-t isect))
	    (begin
	    (is-hit-set! isect #t)
	    (is-t-set! isect t)
	    (is-n-set! isect n)
	    (is-p-set! isect (vec
			       (+ (vx (org ray)) (* (vx (dir ray)) t))
			       (+ (vy (org ray)) (* (vy (dir ray)) t))
			       (+ (vz (org ray)) (* (vz (dir ray)) t))))
	    (is-col-set! isect col)
	    (is-emissiveCol-set! isect emissiveCol))))))))

; pt

(define IMAGE_WIDTH 256)
(define IMAGE_HEIGHT 1)
(define NSUBSAMPLES 2)
(define NPATH_SAMPLES 128)
(define MAX_TRACE_DEPTH 16)

(define (main)
  (let ((myport (open-file-output-port "out.rgb")))
  ;(let ()
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

(define objects
  (list
    (Sphere (vec -1.05 0.0 -2.0) 0.5 (vec 0.75 0.0 0.0) (vec 0.0 0.0 0.0))
    (Sphere (vec 0.0 0.0 -2.0) 0.5 (vec 1.0 1.0 1.0) (vec 1.0 1.0 1.0))
    (Sphere (vec 1.05 0.0 -2.0) 0.5 (vec 0.0 0.0 1.0) (vec 0.0 0.0 0.0))
    (Plane (vec 0.0 -0.5 0.0) (vec 0.0 1.0 0.0) (vec 1.0 1.0 1.0) (vec 0.0 0.0 0.0))))

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

(define (trace ray depth)
  (define (find-nearest obj isect ray)
    (cond
      ((pair? obj) 
       ((car obj) isect ray)
       (find-nearest (cdr obj) isect ray))
      (else isect)))
  (let ((isect (find-nearest objects (make-intersection) ray)))
  (if (is-hit isect)
    ;true - stage2
    (let ((r (random-real)) (phi (* 2.0 PI (random-real))))
      (let 
	((x (* (cos phi) (sqrt (- 1.0 r))))
	 (y (* (sin phi) (sqrt (- 1.0 r))))
	 (z (sqrt r)))
	(receive (b0 b1 b2) (orthoBasis (is-n isect))
	  (let*
	    ((newDir (vec
		       (+ (* x (vx b0)) (* y (vx b1)) (* z (vx b2)))
		       (+ (* x (vy b0)) (* y (vy b1)) (* z (vy b2)))
		       (+ (* x (vz b0)) (* y (vz b1)) (* z (vz b2)))))
	     (eps 0.00001)
	     (newP (vec (+ (vx (is-p isect)) (* eps (vx newDir)))
			(+ (vy (is-p isect)) (* eps (vy newDir))) 
			(+ (vz (is-p isect)) (* eps (vz newDir)))
		     ))
	     (newRay (new-ray newP newDir))
	     (fr (vscale (is-col isect) (/ 1.0 PI)))
	     (cosTheta (vdot newDir (is-n isect)))
	     (Li (trace newRay (+ depth 1)))
	     (Le (is-emissiveCol isect)))
	    ;return
	    (vec (+ (vx Le) (* PI (vx fr) (vx Li))) 
		 (+ (vy Le) (* PI (vy fr) (vy Li))) 
		 (+ (vz Le) (* PI (vz fr) (vz Li))))))))
    (vec 0.7 0.7 0.7) ;false
    )))

(define (pathTrace ray)
  (define (pathTrace-itr num col ray)
    (if (< num NPATH_SAMPLES)
      (pathTrace-itr (+ num 1) (vadd col (trace ray 0)) ray)
      (vscale col (/ 1.0 NPATH_SAMPLES))))
  (pathTrace-itr 0 (vec 0.0 0.0 0.0) ray))

(define (render width height y nsubsamples)
  (let ((img (make-bytevector (* width 4))))
    (define (render-sample-loop i u v sumr sumg sumb)
      (cond
	((= v nsubsamples) (values sumr sumg sumb))
	((= u nsubsamples) (render-sample-loop i 0 (+ v 1) sumr sumg sumb))
	(else
	  (let ((px (/ (+ i (- (/ u nsubsamples) (/ width 2.0))) (/ width 2.0)))
		(py (- 0.0 (/ (+ y (- (/ v nsubsamples) (/ height 2.0))) (/ height 2.0)))))
	    (let ((col (pathTrace (new-ray (vec 0.0 0.0 0.0) (vnormalize (vec px py -1.0))))))
	      (render-sample-loop i (+ u 1) v (+ sumr (vx col)) (+ sumg (vy col)) (+ sumb (vz col))))))))

    (define (render-pixel-loop i)
      (if (= i width)
	img
	(begin
	  (receive (sumr sumg sumb) (render-sample-loop i 0 0 0.0 0.0 0.0)
		   (bytevector-u8-set! img (+ (* i 4) 0) (clamp (/ sumr (square nsubsamples))))
		   (bytevector-u8-set! img (+ (* i 4) 1) (clamp (/ sumg (square nsubsamples))))
		   (bytevector-u8-set! img (+ (* i 4) 2) (clamp (/ sumb (square nsubsamples)))))
	  (render-pixel-loop (+ i 1))))) (render-pixel-loop 0)))

(main)
