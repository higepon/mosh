(library (yuni scheme refimpl arithmetic impl contagion-generic)
         (export contagion/will
                 pcontagion/will
                 econtagion/will
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl coercion)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl arithmetic-util)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl compnum)
                 (yuni scheme refimpl arithmetic impl recnum)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl flonum2rational)
                 (yuni scheme refimpl arithmetic impl rational2flonum)
                 (yuni scheme refimpl arithmetic impl contagion))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Contagion for R5RS-style generic arithmetic, as favored by Will

(define (oops a b retry)
  (error "INTERNAL ERROR in contagion: same-representation arithmetic"
         a b retry)
  #t)

; Algorithm* for arithmetic.  If both are representable as
; integers, convert to bignums and compute, and then convert to inexact.
; Otherwise, convert to flonums and compute.
; One input is a bignum, the other a flonum or compnum.

(define (make-algorithm*c integral-1? integral-2?
			  x->int-1 x->int-2
			  coerce-1 coerce-2)
  (lambda (a b retry)
    (if (and (integral-1? a)
	     (integral-2? b))
	(rational->flonum (retry (x->int-1 a) (x->int-2 b)))
	(retry (coerce-1 a) (coerce-2 b)))))

; One argument is a flonum or a compnum, the other one is a recnum

(define (make-algorithm*cr integral-1? integral-2?
			   real-part-1 imag-part-1
			   real-part-2 imag-part-2
			   x->rec-1 x->rec-2
			   x->comp-1 x->comp-2)
  (lambda (a b retry)
    (if (and (integral-1? (real-part-1 a))
	     (integral-1? (imag-part-1 a))
	     (integral-2? (real-part-2 b))
	     (integral-2? (imag-part-2 b)))
	(let ((a (x->rec-1 a))
	      (b (x->rec-2 b)))
	  (recnum->compnum (retry a b)))
	(let ((a (x->comp-1 a))
	      (b (x->comp-2 b)))
	  (retry a b)))))

#|
(define (always x)
  #t)
|#

(define (zero x)
  (core->flonum 0.0))

; Algorithm* for ordering predicates (<, <=, >, >=): if both are
; representable as integers, represent as bignums and compare. 
; Otherwise represent as flonums and compare.  One of the arguments
; is a bignum, the other is a flonum or compnum.  Compnums with non-zero
; imaginary parts are illegal and flagged as such.

(define (make-algorithm*p integral-1? integral-2?
			  x->int-1 x->int-2
			  float-1? float-2?
			  x->flo-1 x->flo-2)
  (lambda (a b retry)
    (cond
     ((and (integral-1? a)
	   (integral-2? b))
      (retry (x->int-1 a) (x->int-2 b)))
     ((and (float-1? a)
	   (float-2? b))
      (retry (x->flo-1 a) (x->flo-2 b)))
     (else
      (oops a b retry)))))

; Algorithm* for equality predicates.  If both are representable as
; integers, convert to bignums and compare.  Otherwise, convert to
; flonums and compare.
; One input is a bignum, the other a flonum or compnum.

(define (make-algorithm*e integral-1? integral-2?
			  x->int-1 x->int-2
			  coerce-1 coerce-2)
  (lambda (a b retry)
    (if (and (integral-1? a) (integral-2? b))
	(retry (x->int-1 a) (x->int-2 b))
	(retry (coerce-1 a) (coerce-2 b)))))

; Algorithm*e for at least one complex number.

(define (make-algorithm*cre integral-1? integral-2?
			    real-part-1 imag-part-1
			    real-part-2 imag-part-2
			    x->rec-1 x->rec-2
			    x->comp-1 x->comp-2)
  (lambda (a b retry)
    (if (and (integral-1? (real-part-1 a))
	     (integral-1? (imag-part-1 a))
	     (integral-2? (real-part-2 b))
	     (integral-2? (imag-part-2 b)))
        (retry (x->rec-1 a) (x->rec-2 b))
        (retry (x->comp-1 a) (x->comp-2 b)))))

; Standard matrix: for arithmetic operations.

(define cmatrix (make-contagion-matrix))

; Predicate matrix: for <, <=, >, >=
; Algorithm*p handles illegal complex numbers.

(define pmatrix (make-contagion-matrix))

; Equality matrix: for = (only)

(define ematrix (make-contagion-matrix))

(define contagion/will (lambda (a b retry)
			 (do-contagion cmatrix a b retry)))

(define econtagion/will (lambda (a b retry)
			  (do-contagion ematrix a b retry)))

(define pcontagion/will (lambda (a b retry)
			  (do-contagion pmatrix a b retry)))
(define-contagion cmatrix fix fix fixnum->bignum fixnum->bignum) ; $$$
(define-contagion cmatrix fix big fixnum->bignum id)
(define-contagion cmatrix fix rat fixnum->ratnum id)
(define-contagion cmatrix fix rec fixnum->recnum id)
(define-contagion cmatrix fix flo fixnum->flonum id)
(define-contagion cmatrix fix comp fixnum->compnum id)

(define-contagion cmatrix big big oops)
(define-contagion cmatrix big rat bignum->ratnum id)
(define-contagion cmatrix big rec bignum->recnum id)
(define-contagion cmatrix big flo
  (make-algorithm*c always flinteger?
		    id flonum->rational
		    rational->flonum id))
(define-contagion cmatrix big comp
  (make-algorithm*c always compnum-integral? 
		    id compnum->bignum
		    bignum->compnum id))

(define-contagion cmatrix rat rat oops)
(define-contagion cmatrix rat rec ratnum->recnum id)
(define-contagion cmatrix rat flo ratnum->flonum id)
(define-contagion cmatrix rat comp ratnum->compnum id)

(define-contagion cmatrix rec rec oops)
(define-contagion cmatrix rec flo
  (make-algorithm*cr always flinteger?
		     recnum-real recnum-imag
		     id zero
		     id flonum->recnum
		     recnum->compnum flonum->compnum))
(define-contagion cmatrix rec comp
  (make-algorithm*cr always compnum-integral?
		     recnum-real recnum-imag
		     compnum-real compnum-imag
		     id compnum->recnum
		     recnum->compnum id))

(define-contagion cmatrix flo big
  (make-algorithm*c flinteger? always
		    flonum->rational id
		    id rational->flonum))
(define-contagion cmatrix flo flo oops)
(define-contagion cmatrix flo comp flonum->compnum id)

(define-contagion cmatrix comp big
  (make-algorithm*c compnum-integral? always 
		    compnum->bignum id
		    id bignum->compnum))
(define-contagion cmatrix comp comp oops)



(define-contagion pmatrix fix fix fixnum->bignum fixnum->bignum) ; $$$
(define-contagion pmatrix fix big fixnum->bignum id)
(define-contagion pmatrix fix rat fixnum->ratnum id)
(define-contagion pmatrix fix rec fixnum->recnum id)
(define-contagion pmatrix fix flo fixnum->flonum id)
(define-contagion pmatrix fix comp fixnum->compnum id)

(define-contagion pmatrix big big oops)
(define-contagion pmatrix big rat bignum->ratnum id)
(define-contagion pmatrix big rec bignum->recnum id)
(define-contagion pmatrix big flo
  (make-algorithm*p always flinteger?
		    id flonum->bignum
		    always always
		    bignum->flonum id))
(define-contagion pmatrix big comp
  (make-algorithm*p always compnum-integral?
		    id compnum->bignum
		    always compnum-float?
		    bignum->flonum compnum-real))

(define-contagion pmatrix rat rat oops)
(define-contagion pmatrix rat rec ratnum->recnum id)
(define-contagion pmatrix rat flo ratnum->flonum id)
(define-contagion pmatrix rat comp ratnum->compnum id)

(define-contagion pmatrix rec rec oops)
(define-contagion pmatrix rec flo recnum->compnum flonum->compnum)
(define-contagion pmatrix rec comp recnum->compnum id)

(define-contagion pmatrix flo big
  (make-algorithm*p flinteger? always
		    flonum->bignum id
		    always always
		    id bignum->flonum))
(define-contagion pmatrix flo flo oops)
(define-contagion pmatrix flo comp flonum->compnum id)

(define-contagion pmatrix comp big
  (make-algorithm*p compnum-integral? always
		    compnum->bignum id
		    compnum-float? always
		    compnum-real bignum->flonum))
(define-contagion pmatrix comp comp oops)


(define-contagion ematrix fix fix fixnum->bignum fixnum->bignum) ; $$$
(define-contagion ematrix fix big fixnum->bignum id)
(define-contagion ematrix fix rat fixnum->ratnum id)
(define-contagion ematrix fix rec fixnum->recnum id)
(define-contagion ematrix fix flo fixnum->flonum id)
(define-contagion ematrix fix comp fixnum->compnum id)

(define-contagion ematrix big big oops)
(define-contagion ematrix big rat bignum->ratnum id)
(define-contagion ematrix big rec bignum->recnum id)
(define-contagion ematrix big flo
  (make-algorithm*e always flinteger?
		    id flonum->bignum
		    bignum->flonum id))
(define-contagion ematrix big comp
  (make-algorithm*e always compnum-integral?
		    id compnum->bignum
		    bignum->compnum id))

(define-contagion ematrix rat rat oops)
(define-contagion ematrix rat rec ratnum->recnum id)
(define-contagion ematrix rat flo ratnum->flonum id)
(define-contagion ematrix rat comp ratnum->compnum id)

(define-contagion ematrix rec rec oops)
(define-contagion ematrix rec flo
  (make-algorithm*cre exact-integer? flinteger?
		      recnum-real recnum-imag
		      id zero
		      id flonum->recnum
		      recnum->compnum flonum->compnum))
(define-contagion ematrix rec comp
  (make-algorithm*cre exact-integer? flinteger?
		      recnum-real recnum-imag
		      compnum-real compnum-imag
		      id compnum->recnum
		      recnum->compnum id))

(define-contagion ematrix flo big
  (make-algorithm*e flinteger? always
		    flonum->bignum id
		    id bignum->flonum))
(define-contagion ematrix flo rec
  (make-algorithm*cre flinteger? exact-integer?
		      zero id
		      recnum-real recnum-imag
		      flonum->recnum id
		      flonum->compnum recnum->compnum))
(define-contagion ematrix flo flo oops)
(define-contagion ematrix flo comp flonum->compnum id)

(define-contagion ematrix comp big
  (make-algorithm*e compnum-integral? always
		    compnum->bignum id
		    id bignum->compnum))
(define-contagion ematrix comp rec
  (make-algorithm*cre flinteger? exact-integer?
		      compnum-real compnum-imag
		      recnum-real recnum-imag
		      compnum->recnum id
		      id recnum->compnum))
(define-contagion ematrix comp comp oops)


)
