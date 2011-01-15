(library (yuni scheme refimpl arithmetic impl bellerophon)
         (export bellerophon)
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl rational2flonum)
                 (yuni scheme refimpl arithmetic impl flonum2rational)
                 (yuni scheme refimpl arithmetic impl flonum-ieee)
                 )

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; from Larceny

; A version of Algorithm Bellerophon for implementations
; of Scheme that support IEEE double precision arithmetic
; and exact integer arithmetic of unlimited precision.

; #### Mike probably didn't do a great job abstracting this over the
; floating-point precision.

(define n         fl-ieee-mantissa-width)
(define two^n-1   (integer-expt (core->integer 2)
				(integer- n (core->integer 1))))
(define two^n     (integer-expt (core->integer 2) n))
(define p         (integer+ n (core->integer 9)))
(define two^p-1   (integer-expt (core->integer 2)
				(integer- p (core->integer 1))))
(define two^p     (integer-expt (core->integer 2) p))
(define two^p-n-1 (integer-expt (core->integer 2)
				(integer- (integer- p n) (core->integer 1))))
(define two^p-n   (integer-expt (core->integer 2)
				(integer- (integer- p n) (core->integer 1))))

(define flonum:zero (core->flonum 0.0))
(define flonum:infinity flinf+)
(define flonum:minexponent fl-ieee-min-exponent)
(define flonum:minexponent-51 fl-ieee-min-exponent/denormalized)
(define bellerophon:big-f (integer-expt (core->integer 2) p))
(define bellerophon:small-e (flonum->integer
			     (flceiling (fl/ (integer->flonum fl-ieee-min-exponent)
					     (fl/ (fllog (core->flonum 10.0))
						  (fllog (core->flonum 2.0)))))))
(define bellerophon:big-e (flonum->integer
			   (flceiling
			    (fl/ (integer->flonum fl-ieee-max-exponent)
				 (fl/ (fllog (core->flonum 10.0))
				      (fllog (core->flonum 2.0)))))))

(define (bellerophon f e p)
  (cond ((integer-negative? f)
         (integer-negate (bellerophon (integer-negate f) e p)))
	((integer-zero? f) flonum:zero)
	((not (integer=? p n)) (fail0 f e (integer-min p (integer- n (core->integer 1)))))
	((integer<=? bellerophon:big-f f) (fail0 f e p))
	((integer<? e bellerophon:small-e) (fail0 f e p))
	((integer<? bellerophon:big-e e) flonum:infinity)
	((and (integer<? f two^n) (integer>=? e (core->integer 0)) (integer<? e log5-of-two^n))
	 (fl* (integer->flonum f)
	      (integer->flonum (integer-expt (core->integer 10) e))))
	((and (integer<? f two^n) 
	      (integer<? e (core->integer 0))
	      (integer<? (integer-negate e) log5-of-two^n))
	 (fl/ (integer->flonum f)
	      (integer->flonum (integer-expt (core->integer 10) (integer-negate e)))))
	(else (multiply-and-test
	       f e (cond ((integer<? e (core->integer -216)) slop-216)
			 ((integer<? e (core->integer -108)) slop-108)
			 ((integer<? e (core->integer -54))  slop-54)
			 ((integer<? e (core->integer -27))  slop-27)
			 ((integer<? e (core->integer 0))    slop0)
			 ((integer<=? e (core->integer 27))  slop27)
			 ((integer<=? e (core->integer 54))  slop54)
			 ((integer<=? e (core->integer 108)) slop108)
			 ((integer<=? e (core->integer 216)) slop216)
			 (else       slop324))))))
    
(define (multiply-and-test f e slop)
  (let ((x (integer->extended f))
	(y (ten-to-e e)))
    (let ((z (extended-multiply x y)))
      (if (integer<=? (integer-abs (integer- (extended-lowbits z) two^p-n-1)) slop)
	  (fail f e z)
	  (extended->flonum z)))))
    
(define (fail0 f e n) (AlgorithmM f e n))
    
(define (fail f e z) (AlgorithmM f e n))

; Powers of ten are computed from a small table containing
; the best extended precision approximations to
; 10^-216, 10^-108, 10^-54, 10^-27, 10^27, 10^54, 10^108, 10^216.

; The table of powers of ten; see assignments below.
    
(define ten^-216 #f)
(define ten^-108 #f)
(define ten^-54  #f)
(define ten^-27  #f)
(define ten^27   #f)
(define ten^54   #f)
(define ten^108  #f)
(define ten^216  #f)

(define (ten-to-e e)
  (cond ((integer<? e (core->integer -432))
	 (error "Impossible case 1 in bellerophon: " e)
	 #t)
	((integer<? e (core->integer -216))
	 (extended-multiply ten^-216
			    (extended-multiply 
			     ten^-216
			     (ten-to-e (integer+ e (core->integer 432))))))
	((integer<? e (core->integer -108))
	 (extended-multiply ten^-216 (ten-to-e (integer+ e (core->integer 216)))))
	((integer<? e (core->integer -54))
	 (extended-multiply ten^-108 (ten-to-e (integer+ e (core->integer 108)))))
	((integer<? e (core->integer -27))
	 (extended-multiply ten^-54 (ten-to-e (integer+ e (core->integer 54)))))
	((integer<? e (core->integer 0))
	 (extended-multiply ten^-27 (ten-to-e (integer+ e (core->integer 27)))))
	((integer<=? e (core->integer 27)) (integer->extended (integer-expt (core->integer 10) e)))
	((integer<=? e (core->integer 54))
	 (extended-multiply ten^27 (ten-to-e (integer- e (core->integer 27)))))
	((integer<=? e (core->integer 108))
	 (extended-multiply ten^54 (ten-to-e (integer- e (core->integer 54)))))
	((integer<=? e (core->integer 216))
	 (extended-multiply ten^108 (ten-to-e (integer- e (core->integer 108)))))
	((integer<=? e (core->integer 324))
	 (extended-multiply ten^216 (ten-to-e (integer- e (core->integer 216)))))
	(else 
	 (error "Impossible case 2 in bellerophon: " e)
	 #t)))
    
; These slop factors assume that f can be represented exactly
; as an extended precision number, so the slop factor is exactly
; twice the maximum error in the approximation to 10^e.
    
(define slop-216 (core->integer 45))
(define slop-108 (core->integer  9))
(define slop-54  (core->integer  3))
(define slop-27  (core->integer  3))
(define slop0    (core->integer  1))
(define slop27   (core->integer  0))
(define slop54   (core->integer  1))
(define slop108  (core->integer  3))
(define slop216  (core->integer  9))
(define slop324  (core->integer 21))

; Precomputed so we don't have to rely on LOG in initialization phase.
;    (define log5-of-two^n (->exact (ceiling (/ (log two^n) (log 5)))))
(define log5-of-two^n (core->integer 23))
    
; Extended precision floating point, implemented entirely
; in Scheme for portability and ease of maintenance.
;
; The following operations are used directly by Algorithm Bellerophon:
;
;   (integer->extended m)
;   (make-extended m q)
;   (extended->flonum ex)
;   (multiply-extended ex1 ex2)
;   (extended-lowbits ex)
;
; All numbers are positive; negative numbers and zero are
; not supported.
;
; An extended is represented as a pair (x t) where x and t are
; exact integers.  Its value is x * 2^t.  A normalized extended
; is an extended for which 2^{p-1} <= x < 2^p.
    
(define make-extended list)
(define (extended-significand f) (car f))
(define (extended-exponent f) (cadr f))
    
; This flag is set by some operations to indicate whether
; any accuracy was lost during the operation.

; NOTE: This makes this code not thread-safe.  Making all of these
; local definitions internal should fix things.

(define inexact-flag #f)
    
; (expt 2 (- (* 2 p) 1))
(define two^2p-1 (core->integer 170141183460469231731687303715884105728))

; (expt 2 (- (* 2 n) 1))
(define two^2n-1 (core->integer 40564819207303340847894502572032))
    
(define (integer->extended n)
  (cond ((integer<? n two^p-1)
	 (extended-normalize (make-extended n (core->integer 0)) two^p-1))
	((integer>=? n two^p)
	 (extended-normalize-and-round (make-extended n (core->integer 0)) two^p))
	(else (make-extended n (core->integer 0)))))
    
; Given an extended whose significand is less than two^p-1,
; returns the normalized extended having the same value.
; Because two^p-1 is a parameter, this can be used for various
; precisions other than p.
    
(define (extended-normalize f two^p-1)
  (let ((x (extended-significand f))
	(t (extended-exponent f)))
    (if (integer<=? two^p-1 x)
	(begin (set! inexact-flag #f)
	       (make-extended x t))
	(extended-normalize
	 (make-extended (integer* (core->integer 2) x)
			(integer- t (core->integer 1))) 
	 two^p-1))))
    
; Given an extended whose significand is greater than two^p,
; returns the normalized extended closest to the given float,
; rounding to even in case of ties.
; two^p is a parameter so this can be used for different
; precisions.
    
(define (extended-normalize-and-round f two^p)
  (do ((x (extended-significand f) (integer-quotient x (core->integer 2)))
       (guard (core->integer 0) (integer-remainder x (core->integer 2)))
       (sticky (core->integer 0) (integer-max sticky guard))
       (t (extended-exponent f) (integer+ t (core->integer 1))))
      ((integer<? x two^p)
	;; The result is probably inexact.
       ;; This setting will be changed if incorrect.
       (set! inexact-flag #t)
       (cond ((integer-zero? guard)
	      (set! inexact-flag (not (integer-zero? sticky)))
	      (make-extended x t))
	     ((and (integer-zero? sticky) (integer-even? x))
	      (make-extended x t))
	     ((integer=? x (integer- two^p (core->integer 1)))
	      (make-extended (integer-quotient two^p (core->integer 2))
			     (integer+ t (core->integer 1))))
	     (else (make-extended (integer+ x (core->integer 1)) t))))))
    
; Given an extended, round it to the nearest flonum
; (with n bits of precision instead of p).
    
(define (extended->flonum f)
  (let ((ff (extended-normalize-and-round f two^n)))
    (make-float (extended-significand ff)
                (extended-exponent ff))))

; Given normalized extendeds, return their normalized product.

(define (extended-multiply f1 f2)
  (let ((f (integer* (extended-significand f1)
		     (extended-significand f2)))
        (t (integer+ (integer+ (extended-exponent f1)
			       (extended-exponent f2))
		     p)))
    ;; Set flag for most common case.
    (set! inexact-flag #t)
    (if (integer<=? two^2p-1 f)
        (let ((q (integer-quotient f two^p))
              (r (integer-remainder f two^p)))
          (cond ((integer<? r two^p-1)
                 (if (integer-zero? r)
                     (set! inexact-flag #f))
                 (make-extended q t))
                ((integer>? r two^p-1)
                 (make-extended (integer+ q (core->integer 1)) t))
                ((integer-even? q)
                 (make-extended q t))
                (else (make-extended (integer+ q (core->integer 1)) t))))
        (let ((q (integer-quotient f two^p-1))
              (r (integer-remainder f two^p-1)))
          (cond ((integer<? r two^p-1)
                 (if (integer-zero? r)
                     (set! inexact-flag #f))
                 (make-extended q (integer- t (core->integer 1))))
                ((integer>? r two^p-1)
                 (make-extended (integer+ q (core->integer 1))
				(integer- t (core->integer 1))))
                ((integer-even? f)
                 (make-extended q (integer- t (core->integer 1))))
                ((integer=? q (integer- two^p-1 (core->integer 1)))
                 (make-extended two^p-1 t))
                (else (make-extended (integer+ q (core->integer 1)) 
				     (integer- t (core->integer 1)))))))))

(define (extended-lowbits ex)
  (integer-remainder (extended-significand ex)
		     two^p-n))

; End of extended precision number implementation.

; Backup algorithm.
; I'm using an extremely slow backup algorithm, mainly because
; the slow algorithm is needed anyway for denormalized numbers
; and I'm trying to keep things simple.

; Given exact integers f, e, and n, with f nonnegative, returns the
; n-bit precision floating point number closest to f * 10^e.

(define (AlgorithmM f e n)

  (define two^n-1   (integer-expt (core->integer 2) (integer- n (core->integer 1))))
  (define two^n     (integer-expt (core->integer 2) n))
  
  ;; f * 10^e = u/v * 2^k
  
  (define (loop u v k)
    (let ((x (integer-quotient u v)))
      (cond ((and (integer<=? two^n-1 x) (integer<? x two^n))
             (ratio->float u v k))
            ((integer<? x two^n-1)
             (loop (integer* (core->integer 2) u) v (integer- k (core->integer 1))))
            ((integer<=? two^n x)
             (loop u (integer* (core->integer 2) v) (integer+ k (core->integer 1)))))))
  
  (if (integer-negative? e)
      (loop f (integer-expt (core->integer 10) (integer-negate e)) (core->integer 0))
      (loop (integer* f (integer-expt (core->integer 10) e)) 
	    (core->integer 0) (core->integer 1))))

; Given exact positive integers p and q with
; 2^(n-1) <= u/v < 2^n, and exact integer k,
; returns the float closest to u/v * 2^k.

(define (ratio->float u v k)
  (let* ((q (integer-quotient u v))
         (r (integer- u (integer* q v)))
         (v-r (integer- v r)))
    (cond ((integer<? r v-r) (make-float q k))
          ((integer>? r v-r) (make-float (integer+ q (core->integer 1)) k))
          ((integer-zero? (integer-remainder q (core->integer 2))) (make-float q k))
          (else (make-float (integer+ q (core->integer 1)) k)))))

; END OF ALGORITHM MultiplyByTwos

; Primitive operations on flonums.

(define (make-float m q)
  (let loop ((m (if (flonum? m) m (integer->flonum m))) (q q))
    (if (integer<? q flonum:minexponent)
	(loop (fl* (core->flonum .5) m)
	      (integer+ q (core->integer 1)))
	(fl* m
	     (fl-integer-expt (fixnum->flonum (core->integer 2)) q)))))

(define (fl-integer-expt x y)
  (define (recur y)
    (cond ((integer-zero? y)
	   (core->flonum 1.0))
	  ((integer-odd? y)
	   (fl* x (recur (integer- y (core->integer 1)))))
	  (else 
	   (let ((v (recur (integer-quotient y (core->integer 2)))))
	     (fl* v v)))))
  (if (integer>=? y (core->integer 0))
      (recur y)
      (fl/ (core->flonum 1.0) (recur (integer-negate y)))))

(define (slow-ten-to-e e)
  (define (loop1 y s guardbit)
    (cond ((integer<? y two^p)
	   (make-extended (if (integer-zero? guardbit)
			      y
			      (integer+ y (core->integer 1))) s))
	  (else (loop1 (integer-quotient y (core->integer 2))
		       (integer+ s (core->integer 1))
		       (integer-remainder y (core->integer 2))))))
  (define (loop2 y s)
    (cond ((integer<=? two^p-1 y)
	   (make-extended y s))
	  (else (loop2 (integer* (core->integer 2) y)
		       (integer- s (core->integer 1))))))
  (define (loop3 x y n)
    (cond ((integer>=? x (integer* y two^p-1))
	   (loop3-help x y (integer-negate n)))
	  (else (loop3 (integer* (core->integer 2) x) y (integer+ n (core->integer 1))))))
  (define (loop3-help x y n)
    (let* ((q (integer-quotient x y))
	   (r (integer- x (integer* q y)))
	   (y-r (integer- y r)))
      (make-extended (cond ((integer<? r y-r) q)
			   ((integer>? r y-r) (integer+ q (core->integer 1)))
			   ((integer-zero? (integer-remainder q (core->integer 2))) q)
			   (else (integer+ q (core->integer 1))))
		     n)))
  (if (integer-negative? e)
      (loop3 (core->integer 1) 
	     (integer-expt (core->integer 10) (integer-negate e))
	     (core->integer 0))
      (let ((ten^e (integer-expt (core->integer 10) e)))
	(cond ((integer>=? ten^e two^p)
	       (loop1 ten^e (core->integer 0) (core->integer 0)))
	      (else (loop2 ten^e (core->integer 0)))))))

; (set! ten^-216 (slow-ten-to-e (core->integer -216)))
; (set! ten^-108 (slow-ten-to-e (core->integer -108)))
; (set! ten^-54  (slow-ten-to-e (core->integer -54)))
; (set! ten^-27  (slow-ten-to-e (core->integer -27)))
; (set! ten^27   (slow-ten-to-e (core->integer 27)))
; (set! ten^54   (slow-ten-to-e (core->integer 54)))
; (set! ten^108  (slow-ten-to-e (core->integer 108)))
; (set! ten^216  (slow-ten-to-e (core->integer 216)))

; slow-ten-to-e is _really_ slow in Larceny.

; precomputed by slow-ten-to-e
(set! ten^-216 (list (core->integer 12718228212127407597) (core->integer -781)))
(set! ten^-108 (list (core->integer 10830740992659433045) (core->integer -422)))
(set! ten^-54  (list (core->integer 14134776518227074637) (core->integer -243)))
(set! ten^-27  (list (core->integer 11417981541647679048) (core->integer -153)))
(set! ten^27   (list (core->integer 14901161193847656250) (core->integer 26)))
(set! ten^54   (list (core->integer 12037062152420224082) (core->integer 116)))
(set! ten^108  (list (core->integer 15709099088952724970) (core->integer 295)))
(set! ten^216  (list (core->integer 13377742608693866209) (core->integer 654)))

; eof
)
