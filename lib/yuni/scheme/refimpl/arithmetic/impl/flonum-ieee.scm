(library (yuni scheme refimpl arithmetic impl flonum-ieee)
         (export fl-ieee-min-exponent
                 fl-ieee-mantissa-width
                 fl-ieee-min-exponent/denormalized
                 flsign
                 flexponent
                 flsignificand
                 fl-ieee-max-exponent
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl custom)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl integer)
                 )
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; IEEE dependencies

(define fl-ieee-min-exponent/denormalized (core->integer -1074)) ; this includes denormalized numbers
(define fl-ieee-min-exponent (core->integer -1023))
(define fl-ieee-max-exponent (core->integer 1023))
(define core-ieee-mantissa-width 53)
(define fl-ieee-mantissa-width (core->integer core-ieee-mantissa-width))

(define (core-sign x)			      
  (cond			
   ((core:negative? x) -1)
   ((core:positive? x) 0)
   ;; kludge
   ((char=? #\- (string-ref (core:number->string x) 0))
    -1)
   (else 0)))

(define (flsign x)
  (core->integer (core-sign (flonum->core x))))

; This is a kludge to work around a bug in the Scheme 48
; implementation of ->exact on denormalized numbers.

(define (core-abs x)
  (if (core:< x 0.0)
      (core:- x)
      x))

; from Larceny:

(define (core-significand x)
  (if (core:= 0.0 x)
      0
      (let loop ((x (core-abs x)))
	(cond ((and (core:<= .5 x) (core:< x 1.0))
	       (core:inexact->exact
                (core:* x (core:expt 2.0 core-ieee-mantissa-width))))
	      ((core:< x .5) (loop  (core:* 2.0 x)))
	      ((core:<= 1.0 x) (loop (core:* .5 x)))))))

(define (flsignificand x)
  (core->integer (core-significand (flonum->core x))))

(define (core-exponent x)
  (if (core:= 0.0 x)
      0
      (let loop ((x (core-abs x)) (k 0))
	(cond ((and (core:<= .5 x) (core:< x 1.0))
               (core:- k core-ieee-mantissa-width))
	      ((core:< x .5)
               (loop (core:* 2.0 x) (core:- k 1)))
	      ((core:<= 1.0 x)
               (loop (core:* .5 x) (core:+ k 1)))))))

(define (flexponent x)
  (core->integer (core-exponent (flonum->core x))))

)
