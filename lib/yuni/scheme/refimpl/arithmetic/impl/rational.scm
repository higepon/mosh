(library (yuni scheme refimpl arithmetic impl rational)
         (export rational-
                 rational-negative?
                 rational->string
                 rational=?
                 rational/
                 rational+
                 rational*
                 rational<?
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl ratnum))
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic rational arithmetic arithmetic built on top of
; generic integer arithmetic.

(define (exact-rational? p)
  (or (exact-integer? p)
      (ratnum? p)))

(define (rational-numerator p)
  (if (ratnum? p)
      (ratnum-numerator p)
      p))

(define (rational-denominator p)
  (if (ratnum? p)
      (ratnum-denominator p)
      (core->integer 1)))

(define (rational->ratnum r)
  (cond
   ((exact-integer? r) (make-unreduced-ratnum r (core->integer 1)))
   ((ratnum? r) r)))

(define (make-r*r->v op)
  (lambda (p q)
    (op (rational->ratnum p)
	(rational->ratnum q))))

(define rational* (make-r*r->v ratnum*))
(define rational/ (make-r*r->v ratnum/))
(define rational+ (make-r*r->v ratnum+))
(define rational- (make-r*r->v ratnum-))

(define rational<? (make-r*r->v ratnum<?))
(define rational=? (make-r*r->v ratnum=?))

(define (make-r->v op)
  (lambda (p)
    (op (rational->ratnum p))))

(define rational-truncate (make-r->v ratnum-truncate))
(define rational-floor (make-r->v ratnum-floor))

(define rational-positive? (make-r->v ratnum-positive?))
(define rational-negative? (make-r->v ratnum-negative?))

(define (rational->string b r)
  (cond
   ((exact-integer? b) (integer->string b r))
   ((ratnum? b) (ratnum->string b r))))
)
