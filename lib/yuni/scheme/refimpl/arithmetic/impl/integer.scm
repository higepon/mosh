(library (yuni scheme refimpl arithmetic impl integer)
         (export core->integer
                 integer->string
                 integer<?
                 integer-
                 integer-quotient
                 integer=?
                 integer-abs
                 integer-negative?
                 integer-positive?
                 integer*
                 integer+
                 integer-negate
                 integer-gcd
                 exact-integer?
                 integer>=?
                 integer->core
                 integer>?
                 integer<=?
                 integer-expt
                 integer-quotient+remainder
                 integer-zero?
                 integer-even?
                 integer-max
                 integer-odd?
                 integer-remainder
                 integer-min
                 integer->bignum
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl custom)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl bigbit)
                 )
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic arbitrary-precision integer arithmetic built on top of
; fixnums and bignums.

(define (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))

(define (integer->core m)
  (if (fixnum? m)
      (fixnum->core m)
      (bignum->core m)))

(define fixnum-least-r5rs (fixnum->core (least-fixnum)))
(define fixnum-greatest-r5rs (fixnum->core (greatest-fixnum)))

(define (core->integer m)
  (if (and (core:>= m fixnum-least-r5rs)
	   (core:<= m fixnum-greatest-r5rs))
      (core->fixnum m)
      (core->bignum m)))

(define (integer->bignum m)
  (if (bignum? m)
      m
      (fixnum->bignum m)))

(define (make-int*int->val op)
  (lambda (a b)
    (op (integer->bignum a) (integer->bignum b))))

(define integer+ (make-int*int->val bignum+))
(define (integer- m n)
  (integer+ m (integer-negate n)))

; ####assumes symmetric fixnum range
(define (integer-negate m)
  (cond ((bignum? m)
	 (bignum-negate m))
	((fixnum=? m (least-fixnum))
	 least-fixnum-negated)
	(else (fixnum- m))))

; ####assumes two's complement---oops!
(define least-fixnum-negated (bignum-negate (fixnum->bignum (least-fixnum))))

(define integer* (make-int*int->val bignum*))
(define integer-quotient (make-int*int->val bignum-quotient))
(define integer-remainder (make-int*int->val bignum-remainder))
(define integer-quotient+remainder (make-int*int->val bignum-quotient+remainder))

(define integer=? (make-int*int->val bignum=?))
(define integer<? (make-int*int->val bignum<?))

; GCD

(define (integer-zero? x)
  (if (fixnum? x)
      (fixnum-zero? x)
      (bignum-zero? x)))

(define (integer-gcd x y)
  (cond ((integer<? x (core->integer 0)) (integer-gcd (integer-negate x) y))
	((integer<? y (core->integer 0)) (integer-gcd x (integer-negate y)))
	((integer<? x y) (euclid y x))
	(else (euclid x y))))

(define (euclid x y)
  (if (integer-zero? y)
      x
      (euclid y (integer-remainder x y))))

; LCM

(define (integer-lcm x y)
  (let ((g (integer-gcd x y)))
    (if (integer-zero? g)
	g
	(integer* (integer-quotient (integer-abs x) g)
		  (integer-abs y)))))
#| ;; dupe??
(define (integer-abs x)
  (if (integer<? x (core->integer 0))
      (integer-negate x)
      x))
|#

(define (integer-expt x y)
  (cond ((integer-zero? y)
	 (core->integer 1))
	((integer-odd? y)
	 (integer* x (integer-expt x (integer- y (core->integer 1)))))
	(else 
	 (let ((v (integer-expt x (integer-quotient y (core->integer 2)))))
	   (integer* v v)))))

(define (integer-even? n)
  (integer-zero? (integer-remainder n (core->integer 2))))

(define (integer-odd? n)
  (not (integer-even? n)))

(define (integer>? m n)
  (integer<? n m))

(define (integer>=? m n)
  (not (integer<? m n)))

(define (integer<=? m n)
  (integer>=? n m))

(define (integer-negative? m)
  (integer<? m (core->integer 0)))
(define (integer-positive? m)
  (integer>? m (core->integer 0)))

(define (integer-min m n)
  (if (integer<? m n) m n))
(define (integer-max m n)
  (if (integer>? m n) m n))

(define (integer-abs m)
  (if (integer-negative? m)
      (integer-negate m)
      m))

(define (integer->string b r)
  (bignum->string (integer->bignum b) r))

(define (integer-bitwise-not m)
  (if (fixnum? m)
      (fixnum-not m)
      (bignum-not m)))

(define (make-binary-bitwise-op fix-op big-op)
  (lambda (a b)
    (if (fixnum? a)
	(if (fixnum? b)
	    (fix-op a b)
	    (big-op (fixnum->bignum a) b))
	(if (fixnum? b)
	    (big-op a (fixnum->bignum b))
	    (big-op a b)))))

(define integer-bitwise-ior
  (make-binary-bitwise-op fixnum-ior bignum-ior))
(define integer-bitwise-xor
  (make-binary-bitwise-op fixnum-xor bignum-xor))
(define integer-bitwise-and
  (make-binary-bitwise-op fixnum-and bignum-and))
(define integer-arithmetic-shift-left (make-int*int->val bignum-arithmetic-shift-left))

)
