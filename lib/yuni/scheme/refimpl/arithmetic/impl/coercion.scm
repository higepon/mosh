(library (yuni scheme refimpl arithmetic impl coercion)
         (export bignum->flonum
                 recnum->compnum
                 compnum->recnum
                 ratnum->flonum
                 compnum->bignum
                 bignum->compnum
                 flonum->compnum
                 flonum->recnum
                 flonum->bignum
                 ratnum->compnum
                 ratnum->recnum
                 bignum->recnum
                 bignum->ratnum
                 fixnum->compnum
                 fixnum->recnum
                 fixnum->ratnum
                 compnum-integral?
                 compnum-float?
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl compnum)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 (yuni scheme refimpl arithmetic impl recnum)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl rational2flonum)
                 (yuni scheme refimpl arithmetic impl flonum2rational)
                 )
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Numeric coercion

(define (fixnum->ratnum f)
  (make-unreduced-ratnum f (core->integer 1)))

(define (fixnum->recnum f)
  (make-recnum f (core->integer 0)))

(define (fixnum->compnum f)
  (make-compnum (fixnum->flonum f) (core->flonum 0.0)))

(define (bignum->ratnum f)
  (make-unreduced-ratnum f (core->integer 1)))

(define (bignum->recnum f)
  (make-recnum f 0))

(define (bignum->compnum f)
  (make-compnum (integer->flonum f) (core->flonum 0.0)))

(define (ratnum->recnum f)
  (make-recnum f (core->integer 0)))

(define (ratnum->flonum f)
  (rational->flonum f))

(define (ratnum->compnum f)
  (make-compnum (ratnum->flonum f) (core->flonum 0.0)))

(define (recnum->compnum f)
  (make-compnum (rational->flonum (recnum-real f)) 
		(rational->flonum (recnum-imag f))))

(define (flonum->compnum f)
  (make-compnum f (core->flonum 0.0)))

(define (flonum->bignum f)
  (integer->bignum (flonum->integer f)))

(define (compnum->bignum f)
  (flonum->bignum (compnum-real f)))

(define (compnum->integer c)
  (if (flzero? (compnum-imag c))
      (flonum->integer (compnum-real c))
      (error "expected compnum with zero imaginary part" c)))

(define (flonum->recnum f)
  (ratnum->recnum (flonum->rational f)))

(define (compnum->recnum f)
  (make-recnum (flonum->rational (compnum-real f))
	       (flonum->rational (compnum-imag f))))

(define (bignum->flonum f)
  (integer->flonum f))

(define (compnum-float? f)
  (flzero? (compnum-imag f)))

#| dupe??
(define (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))
|#

(define (compnum-integral? f)
  (and (compnum-float? f)
       (flinteger? (compnum-real f))))

(define (recnum-integral? f)
  (and (exact-integer? (recnum-real f))
       (integer-zero? (recnum-imag f))))

(define (id x) x)

; eof
)
