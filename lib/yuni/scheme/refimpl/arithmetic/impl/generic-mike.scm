; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic arithmetic, as preferred by Mike

(define (number? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (recnum? obj)
      (flonum? obj)
      (compnum? obj)))

(define (complex? obj)
  (number? obj))

(define (real? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (flonum? obj)))

(define (rational? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (and (flonum? obj)
	   (not (or (= obj flinf+)
		    (= obj flinf-)
		    (flnan? obj))))))

(define (integer? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (and (flonum? obj)
	   (flinteger? obj))
      (and (compnum? obj)
	   (compnum-integral? obj))))

(define (exact? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (recnum? obj)))

(define (inexact? obj)
  (or (flonum? obj)
      (compnum? obj)))

(define-syntax define-binary
  (syntax-rules ()
    ((define-binary ?name ?contagion ?bignum-op ?ratnum-op ?recnum-op)
     (define (?name a b)
       (cond
	((bignum? a)
	 (if (bignum? b)
	     (?bignum-op a b)
	     (?contagion a b ?name)))
	((ratnum? a)
	 (if (ratnum? b)
	     (?ratnum-op a b)
	     (?contagion a b ?name)))
	((recnum? a)
	 (if (recnum? b)
	     (?recnum-op a b)
	     (?contagion a b ?name)))
	(else
	 (?contagion a b ?name)))))
    ((define-binary ?name ?contagion ?fixnum-op ?bignum-op ?ratnum-op ?recnum-op)
     (define (?name a b)
       (cond
	((fixnum? a)
	 (if (fixnum? b)
	     (?fixnum-op a b)
	     (?contagion a b ?name)))
	((bignum? a)
	 (if (bignum? b)
	     (?bignum-op a b)
	     (?contagion a b ?name)))
	((ratnum? a)
	 (if (ratnum? b)
	     (?ratnum-op a b)
	     (?contagion a b ?name)))
	((recnum? a)
	 (if (recnum? b)
	     (?recnum-op a b)
	     (?contagion a b ?name)))
	(else
	 (?contagion a b ?name)))))))

(define-binary =/2 econtagion/mike
  fixnum=? bignum=? ratnum=? recnum=?)

(define-binary </2 pcontagion/mike
  fixnum<? bignum<? ratnum< (make-typo-op/2 < 'rational))
(define-binary <=/2 pcontagion/mike
  fixnum<? bignum<=? ratnum<=? (make-typo-op/2 <= 'rational))
(define-binary >=/2 pcontagion/mike
  fixnum>=? bignum>=? ratnum>=? (make-typo-op/2 >= 'rational))
(define-binary >/2 pcontagion/mike
  fixnum>=? bignum>? ratnum>? (make-typo-op/2 > 'rational))

(define = (make-transitive-pred =/2))
(define < (make-transitive-pred </2))
(define <= (make-transitive-pred <=/2))
(define >= (make-transitive-pred >=/2))
(define > (make-transitive-pred >=/2))

(define-syntax define-unary
  (syntax-rules ()
    ((define-unary ?name ?fixnum-op ?bignum-op ?ratnum-op ?recnum-op)
     (define (?name a)
       (cond
	((fixnum? a)
	 (?fixnum-op a))
	((bignum? a)
	 (?bignum-op a))
	((ratnum? a)
	 (?ratnum-op a))
	((recnum? a)
	 (?recnum-op a))
	(else
	 (?name (inexact->exact a))))))))

(define-unary zero? fixnum-zero? bignum-zero? never never)
(define-unary positive? fixnum-positive? bignum-positive? ratnum-positive?
  (make-typo-op/1 positive? 'rational))
(define-unary negative? fixnum-negative? bignum-negative? ratnum-negative?
  (make-typo-op/1 negative? 'rational))
(define-unary odd? fixnum-odd? bignum-odd?
  (make-typo-op/1 odd? 'integer)
  (make-typo-op/1 odd? 'integer))
(define-unary even? fixnum-even? bignum-even?
  (make-typo-op/1 even? 'integer)
  (make-typo-op/1 even? 'integer))

(define min (make-min/max <))
(define max (make-min/max >))

(define-binary plus/2 contagion/mike
  bignum+ ratnum+ recnum+)
(define-binary minus/2 contagion/mike
  bignum- ratnum- recnum-)
(define-binary */2 contagion/mike
  bignum* ratnum* recnum*)
(define-binary //2 contagion/mike
  integer/ integer/ ratnum/ recnum/)

(define (+ . args)
  (reduce (core->integer 0) plus/2 args))
(define (- arg0 . args)
  (reduce (core->integer 0) minus/2 (cons arg0 args)))
(define (* . args)
  (reduce (core->integer 1) */2 args))
(define (/ arg0 . args)
  (reduce (core->integer 1) //2 (cons arg0 args)))

;; ABS is evil ...
(define *minus-least-fixnum* (bignum-negate (fixnum->bignum (least-fixnum))))

(define (fixnum-abs x)
  (cond
   ((fixnum-negative? x)
    (if (fixnum=? x (least-fixnum))
        *minus-least-fixnum*
	(fixnum- x)))
   (else x)))

(define-unary abs
  fixnum-abs bignum-abs ratnum-abs
  (make-typo-op/1 abs 'rational))

(define-binary quotient icontagion/mike
  fixnum-quotient
  bignum-quotient
  (make-typo-op/2 quotient 'integer)
  (make-typo-op/2 quotient 'integer))
  
(define-binary remainder icontagion/mike
  fixnum-remainder
  bignum-remainder
  (make-typo-op/2 remainder 'integer)
  (make-typo-op/2 remainder 'integer))

(define-binary quotient+remainder icontagion/mike
  fixnum-quotient+remainder
  bignum-quotient+remainder
  (make-typo-op/2 quotient+remainder 'integer)
  (make-typo-op/2 quotient+remainder 'integer))

(define (modulo x y)
  (if (and (integer? x) (integer? y))
      (let* ((q (quotient x y))
	     (r (- x (* q y))))
	(cond ((zero? r)
	       r)
	      ((negative? r)
	       (if (negative? y)
		   r
		   (+ r y)))
	      ((negative? y)
	       (+ r y))
	      (else
	       r)))
      (error "modulo expects integral arguments" x y)))

; from "Cleaning up the Tower"

(define (div+mod x y)
  (let* ((div
	  (cond
	   ((positive? y)
	    (let ((n (* (numerator x)
			(denominator y)))
		  (d (* (denominator x)
			(numerator y))))
	      (if (negative? n)
		  (- (quotient (- (- d n) 1) d))
		  (quotient n d))))
	   ((zero? y)
	    0)
	   ((negative? y)
	    (let ((n (* -2 
			(numerator x)
			(denominator y)))
		  (d (* (denominator x)
			(- (numerator y)))))
	      (if (< n d)
		  (- (quotient (- d n) (* 2 d)))
		  (quotient (+ n d -1) (* 2 d)))))))
	 (mod
	  (- x (* div y))))
    (values div mod)))

(define (div x y)
  (call-with-values
      (lambda () (div+mod x y))
    (lambda (d m)
      d)))

(define (mod x y)
  (call-with-values
      (lambda () (div+mod x y))
    (lambda (d m)
      m)))

(define (gcd/2 x y)
  (if (and (integer? x) (integer? y))
      (cond ((< x (core->integer 0)) (gcd/2 (- x) y))
	    ((< y (core->integer 0)) (gcd/2 x (- y)))
	    ((< x y) (euclid y x))
	    (else (euclid x y)))
      (error "gcd expects integral arguments" x y)))

(define (euclid x y)
  (if (zero? y)
      x
      (euclid y (remainder x y))))

(define (lcm/2 x y)
  (let ((g (gcd/2 x y)))
    (if (zero? g)
	g
	(* (quotient (abs x) g)
	   (abs y)))))

(define (gcd . args)
  (reduce (core->integer 0) gcd/2 args))

(define (lcm . args)
  (reduce (core->integer 1) lcm/2 args))

(define-unary numerator
  id id ratnum-numerator
  (make-typo-op/1 numerator 'rational))

(define-unary denominator
  one one ratnum-denominator
  (make-typo-op/1 denominator 'rational))

;; floor is primitive
(define-unary floor
  id id ratnum-floor
  (make-typo-op/1 floor 'rational))

(define (floor x)
  (cond
   ((fixnum? x) x)
   ((bignum? x) x)
   ((ratnum? x) (ratnum-floor x))
   ((recnum? x) ((make-typo-op/1 floor 'rational) x))
   ((flonum? x) (flfloor x))
   ((compnum? x) ((make-typo-op/1 floor 'rational) x))))

(define (ceiling x)
  (- (floor (- x))))

(define (truncate x)
  (if (negative? x)
      (ceiling x)
      (floor x)))

(define (round x)
  (let* ((x+1/2 (+ x (core->ratnum 1/2)))
	 (r (floor x+1/2)))
    (if (and (= r x+1/2)
	     (odd? r))
	(- r (core->integer 1))
	r)))

(define (expt x y)

  (define (e x y)
    (cond ((zero? y)
	   (core->integer 1))
	  ((odd? y)
	   (* x (e x (- y (core->integer 1)))))
	  (else 
	   (let ((v (e x (quotient y (core->integer 2)))))
	     (* v v)))))

  (cond ((zero? x)
	 (if (zero? y)
	     (core->integer 1)
	     (core->integer 0)))
	((integer? y)
	 (if (negative? y)
	     (/ (expt x (- y)))
	     (e x y)))
	(else
	 (error "expt expects integer power" y))))

(define (make-rectangular a b)
  (if (and (flonum? a) (flonum? b))
      (make-compnum a b)
      (rectangulate (x->rational a) (x->rational b))))

(define (x->rational x)
  (cond
   ((fixnum? x) x)
   ((bignum? x) x)
   ((ratnum? x) x)
   ((flonum? x) (flonum->rational x))
   (else
    (error "rational expected" x))))

(define (real-part x)
  (cond
   ((or (fixnum? x)
	(bignum? x)
	(ratnum? x))
    x)
   ((recnum? x) (recnum-real x))
   ((flonum? x) x)
   ((compnum? x) (compnum-real x))
   (else
    (error "real-part: invalid argument" x))))

(define (imag-part x)
  (cond
   ((or (fixnum? x)
	(bignum? x)
	(ratnum? x))
    (core->integer 0))
   ((recnum? x) (recnum-imag x))
   ((flonum? x) (core->integer 0))
   ((compnum? x) (compnum-imag x))
   (else
    (error "imag-part: invalid argument" x))))
    
; The revised SRFI 77 does not specify these, and their names
; are in conflict with procedures defined in r5rs.sch.

;(define-unary bitwise-not fixnum-not bignum-not
;  (make-typo-op/1 bitwise-not 'integer)
;  (make-typo-op/1 bitwise-not 'integer))

;(define-binary bitwise-ior/2 fixnum-ior bignum-ior
;  (make-typo-op/2 bitwise-ior/2 'integer)
;  (make-typo-op/2 bitwise-ior/2 'integer))

;(define-binary bitwise-xor/2 fixnum-xor bignum-xor
;  (make-typo-op/2 bitwise-xor/2 'integer)
;  (make-typo-op/2 bitwise-xor/2 'integer))

;(define-binary bitwise-and/2 fixnum-and bignum-and
;  (make-typo-op/2 bitwise-and/2 'integer)
;  (make-typo-op/2 bitwise-and/2 'integer))

;(define (bitwise-ior . args)
;  (reduce (core->integer 0) bitwise-ior/2 args))
;(define (bitwise-and . args)
;  (reduce (core->integer 1) bitwise-and/2 args))
;(define (bitwise-xor . args)
;  (reduce (core->integer 1) bitwise-xor/2 args))

(define (arithmetic-shift-left a b)

  (define (fail)
    (error "arithmetic-shift-left expects exact integer arguments" a b))

  (cond
   ((fixnum? a)
    (cond
     ((fixnum? b)
      (bignum-arithmetic-shift-left (fixnum->bignum a) (fixnum->bignum b)))
     ((bignum? b)
      (bignum-arithmetic-shift-left (fixnum->bignum a) b))
     (else (fail))))
   ((bignum? a)
    (cond
     ((fixnum? b)
      (bignum-arithmetic-shift-left a (fixnum->bignum b)))
     ((bignum? b)
      (bignum-arithmetic-shift-left a (fixnum->bignum b)))
     (else (fail))))))

(define (exact->inexact x)
  (cond
   ((fixnum? x) (fixnum->flonum x))
   ((bignum? x) (bignum->flonum x))
   ((ratnum? x) (ratnum->flonum x))
   ((recnum? x) (recnum->compnum x))
   ((flonum? x) x)
   ((compnum? x) x)
   (else
    (error "exact->inexact expects a numerical argument" x))))

(define (number->flonum x)
  (cond
   ((fixnum? x) (fixnum->flonum x))
   ((bignum? x) (bignum->flonum x))
   ((ratnum? x) (ratnum->flonum x))
   ((recnum? x)
    (error "number->flonum expects argument with non-zero imaginary part" x))
   ((flonum? x) x)
   ((compnum? x)
    (if (flzero? (compnum-imag x))
	(compnum-real x)
	(error "number->flonum expects argument with non-zero imaginary part" x)))
   (else
    (error "number->flonum expects a numerical argument" x))))

(define (compnum->exact a)
  (make-rectangular (inexact->exact (real-part a))
		    (inexact->exact (imag-part a))))

(define (inexact->exact x)
  (cond
   ((fixnum? x) x)
   ((bignum? x) x)
   ((ratnum? x) x)
   ((recnum? x) x)
   ((flonum? x) (flonum->rational x))
   ((compnum? x) (compnum->exact x))
   (else
    (error "inexact->exact expects a numerical argument" x))))

; Simplest rational within an interval.  Copied from IEEE P1178/D4 nimpl.tex.

(define (rationalize x e)
  (let ((e (abs e)))
    (simplest-rational (- x e) (+ x e))))

(define (simplest-rational x y)
  (define (simplest-rational-internal x y)
    ;; assumes 0 < X < Y
    (let ((fx (floor x))
          (fy (floor y)))
      (cond ((not (< fx x))
             fx)
            ((= fx fy)
             (+ fx
		(/ (core->integer 1) 
		   (simplest-rational-internal
		    (/ (core->integer 1) (- y fy))
		    (/ (core->integer 1) (- x fx))))))
            (else
             (+ (core->integer 1) fx)))))
  ;; Do some juggling to satisfy preconditions of simplest-rational-internal.
  (cond ((not (< x y))
         (if (rational? x) x (error "(rationalize <irrational> 0)" x)))
        ((positive? x)
         (simplest-rational-internal x y))
        ((negative? y)
         (- (core->integer 0)
	    (simplest-rational-internal (- (core->integer 0) y) (- (core->integer 0) x))))
        (else
	 (core->integer 0))))

