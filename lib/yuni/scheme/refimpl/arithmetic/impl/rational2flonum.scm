(library (yuni scheme refimpl arithmetic impl rational2flonum)
         (export integer->flonum
                 rational->flonum
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 (yuni scheme refimpl arithmetic impl rational)
                 (yuni scheme refimpl arithmetic impl flonum))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; from Scheme 48

(define (integer->flonum k)
  (define *modulus* (fixnum- (greatest-fixnum) (fx 1)))
  (or (maybe-fixnum->flonum k)
      (fl+ (fl* (fixnum->flonum *modulus*)
		(integer->flonum (integer-quotient k *modulus*)))
	   (fixnum->flonum (integer-remainder k *modulus*)))))

(define (maybe-fixnum->flonum k)    ;Returns #f is k is a bignum
  (if (fixnum? k)
      (fixnum->flonum k)
      #f))

; end from Scheme 48

; from Larceny

; What ->INEXACT should do when given an exact rational.
;
; Assumes inexact reals are represented using IEEE double precision
; floating point.  IEEE single and extended precision can be handled
; by changing n and flonum:minexponent.
;
; Test case: (->inexact 14285714285714285714285) should be
; 1.4285714285714286e22, not 1.4285714285714284e22.

; #### (SEE WILL'S NOTES)
(define n (fx 53))
(define two^n-1 (integer-expt (core->integer 2) (integer- n (core->integer 1))))
(define two^n (integer-expt (core->integer 2) n))
(define flonum:minexponent
  (integer-negate (integer- (integer-expt (core->integer 2) (fx 10)) (core->integer 1))))

(define hard-case
  ;; r = u/v * 2^k
  (letrec ((loop
	    (lambda (u v k)
	      (let ((x (integer-quotient u v)))
		(cond ((and (integer<=? two^n-1 x) (integer<? x two^n))
		       (ratio->float u v k))
		      ((integer<? x two^n-1)
		       (loop (integer* (core->integer 2) u) v (integer- k (core->integer 1))))
		      ((integer<=? two^n x)
		       (loop u (integer* 2 v) (integer+ k (core->integer 1)))))))))

    (lambda (r)
      (let ((p (ratnum-numerator r))
	    (q (ratnum-denominator r)))
	(let ((k (integer- (ratnum-log2 r) n)))
	  (if (integer>? k (core->integer 0))
	      (loop p (integer* q (integer-expt (core->integer 2) k)) k)
	      (loop (integer* p (integer-expt (core->integer 2) (integer-negate k))) q k)))))))


; Given exact positive integers u and v with
; 2^(n-1) <= u/v < 2^n, and exact integer k,
; returns the float closest to u/v * 2^k.

(define (ratio->float u v k)
  (let* ((q (integer-quotient u v))
         (r (integer- u (integer* q v)))
         (v-r (integer- v r)))
    (cond ((rational<? r v-r) (make-float q k))
          ((rational<? v-r r) (make-float (integer+ q (core->integer 1)) k))
          ((integer-zero? (integer-remainder q (core->integer 2))) (make-float q k))
          (else (make-float (integer+ q (core->integer 1)) k)))))

; Primitive operations on flonums.

(define (make-float m q)
  (let ((m (if (flonum? m) m (integer->flonum m))))
    (if (integer<? q flonum:minexponent)
	(make-float (fl* (core->flonum .5) m)
		    (integer+ q (core->integer 1)))
	(fl* m
	     (flinteger-expt (fixnum->flonum (core->integer 2)) q)))))

(define (rational->flonum r)
  (cond
   ((exact-integer? r)
    (integer->flonum r))
   ((rational<? r (core->integer 0))
    (fl- (core->flonum 0.0) (rational->flonum (ratnum-abs r))))
   (else
    (let ((p (ratnum-numerator r))
	  (q (ratnum-denominator r)))
      (cond ((and (integer<=? p two^n)
		  (integer<=? q two^n))
	     (fl/ (integer->flonum p)
		  (integer->flonum q)))
	    (else
	     (hard-case r)))))))

; Integer-length, a la Common Lisp, written in portable Scheme.

; from Scheme 48

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))
(define head car)
(define (tail s) (force (cdr s)))

(define integer-length
  (let ()
    (define useful
      (let loop ((p (integer-expt (core->integer 2) (fx 8))) (n (fx 4)))
	(cons-stream (cons p n)
		     (loop (integer* p p) (integer* n (core->integer 2))))))
    
    (define upto-16
      (vector (fx 0) (fx 1) 
	      (fx 2) (fx 2)
	      (fx 3) (fx 3) (fx 3) (fx 3)
	      (fx 4) (fx 4) (fx 4) (fx 4) (fx 4) (fx 4) (fx 4) (fx 4)))
    
    (define (recur n)
      (if (integer<? n (fx 16))
	  (vector-ref upto-16 (integer->core n))
	  (let loop ((s useful) (prev (fx 16)))
	    (let ((z (head s)))
	      (if (integer<? n (car z))
		  (integer+ (cdr z) (recur (integer-quotient n prev)))
		  (loop (tail s) (car z)))))))
    (define (integer-length n)
      (if (integer<? n (core->integer 0))
	  (recur (integer- (core->integer -1) n))
	  (recur n)))

    integer-length))

; end from Scheme 48

; assumes r is positive
; we want (->exact (ceiling (log2 r)))
(define (ratnum-log2 r)
  (let ((n (ratnum-numerator r))
	(d (ratnum-denominator r)))
    (let* ((approx
	    (integer- (integer-length n) (integer-length d)))
	   (power-0 (integer-expt (core->integer 2) (integer- approx (core->integer 1))))
	   (power-1 (integer* (core->integer 2) power-0))
	   (power-2 (integer* (core->integer 2) power-1)))
      (cond
       ((rational<? r power-0) (integer- approx (core->integer 1)))
       ((rational<? r power-1) approx)
       (else (integer+ approx (core->integer 1)))))))

(define (flinteger-expt x y)
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
)
