(library (yuni scheme refimpl arithmetic impl generic)
         (export 
           number?
           complex?
           real?
           real-valued?
           rational?
           rational-valued?
           integer?
           integer-valued?
           exact?
           inexact?
           = < <= >= >
           zero?
           positive?
           negative?
           odd?
           even?
           nan?
           finite?
           infinite?
           min
           max
           + - * /
           abs
           quotient
           remainder
           modulo
           div+mod
           div
           mod
           gcd
           lcm
           numerator
           denominator
           floor
           ceiling
           truncate
           round
           exp
           sin
           cos
           tan
           asin
           acos
           atan
           log
           sqrt
           expt
           make-rectangular
           make-polar
           real-part
           imag-part
           magnitude
           angle
           (rename (x->inexact inexact)
                   (x->exact exact))
           rationalize
           exact-rational?)
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl contagion-generic)
                 (yuni scheme refimpl arithmetic impl arithmetic-util)
                 (yuni scheme refimpl arithmetic impl flonum2rational)
                 (yuni scheme refimpl arithmetic impl rational2flonum)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl coercion)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 (yuni scheme refimpl arithmetic impl recnum)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl compnum))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic, R5RS-style arithmetic, as preferred by Will

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

(define (real-valued? obj)
  (or (real? obj)
      (and (compnum? obj)
	   (zero? (compnum-imag obj)))))

(define (rational? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (and (flonum? obj)
	   (not (or (= obj flinf+)
		    (= obj flinf-)
		    (flnan? obj))))))

(define (rational-valued? obj)
  (or (rational? obj)
      (and (compnum? obj)
	   (zero? (compnum-imag obj))
	   (rational? (compnum-real obj)))))

(define (integer? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (and (flonum? obj)
	   (flinteger? obj))))

(define (integer-valued? obj)
  (or (integer? obj)
      (and (compnum? obj)
	   (zero? (compnum-imag obj))
	   (integer? (compnum-real obj)))))

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
    ((define-binary ?name ?contagion
       ?bignum-op ?ratnum-op ?recnum-op ?flonum-op ?compnum-op)
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
	((flonum? a)
	 (if (flonum? b)
	     (?flonum-op a b)
	     (?contagion a b ?name)))
	((compnum? a)
	 (if (compnum? b)
	     (?compnum-op a b)
	     (?contagion a b ?name)))
	(else
	 (?contagion a b ?name)))))
    ((define-binary ?name ?contagion
       ?fixnum-op ?bignum-op ?ratnum-op ?recnum-op ?flonum-op ?compnum-op)
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
	((flonum? a)
	 (if (flonum? b)
	     (?flonum-op a b)
	     (?contagion a b ?name)))
	((compnum? a)
	 (if (compnum? b)
	     (?compnum-op a b)
	     (?contagion a b ?name)))
	(else
	 (?contagion a b ?name)))))))

(define-binary =/2 econtagion/will
  fixnum=? bignum=? ratnum=? recnum=? fl=? compnum=?)

(define-binary </2 pcontagion/will
  fixnum<? bignum<? ratnum<? (make-typo-op/2 < 'real)
  fl<? (make-typo-op/2 < 'real))
(define-binary <=/2 pcontagion/will
  fixnum<? bignum<=? ratnum<=? (make-typo-op/2 <= 'real)
  fl<? (make-typo-op/2 <= 'real))
(define-binary >=/2 pcontagion/will
  fixnum>=? bignum>=? ratnum>=? (make-typo-op/2 >= 'real)
  fl>=? (make-typo-op/2 >= 'real))
(define-binary >/2 pcontagion/will
  fixnum>=? bignum>? ratnum>? (make-typo-op/2 > 'real)
  fl>?
  (make-typo-op/2 > 'real))

(define = (make-transitive-pred =/2))
(define < (make-transitive-pred </2))
(define <= (make-transitive-pred <=/2))
(define >= (make-transitive-pred >=/2))
(define > (make-transitive-pred >/2))

(define-syntax define-unary
  (syntax-rules ()
    ((define-unary ?name ?fixnum-op ?bignum-op ?ratnum-op ?recnum-op
                         ?flonum-op ?compnum-op)
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
	((flonum? a)
	 (?flonum-op a))
	((compnum? a)
	 (?compnum-op a))
	(else
	 (error "expects a numerical argument" ?name a)))))))

(define-unary zero? fixnum-zero? never never never flzero? compnum-zero?)
(define-unary positive? fixnum-positive? bignum-positive? ratnum-positive?
  (make-typo-op/1 positive? 'real)
  flpositive?
  (make-typo-op/1 positive? 'real))  
  
(define-unary negative? fixnum-negative? bignum-negative? ratnum-negative?
  (make-typo-op/1 negative? 'real)
  flnegative?
  (make-typo-op/1 negative? 'real))

(define-unary odd? fixnum-odd? bignum-odd?
  (make-typo-op/1 odd? 'integer)
  (make-typo-op/1 odd? 'integer)
  flodd?
  (make-typo-op/1 odd? 'integer))

(define-unary even? fixnum-even? bignum-even?
  (make-typo-op/1 even? 'integer)
  (make-typo-op/1 even? 'integer)
  fleven?
  (make-typo-op/1 even? 'integer))

(define-unary nan? never never never
  (make-typo-op/1 nan? 'real)
  flnan?
  (make-typo-op/1 nan? 'real))
(define-unary finite?
  always always always
  (make-typo-op/1 finite? 'real)
  flfinite?
  (make-typo-op/1 finite? 'real))
(define-unary infinite? never never never
  (make-typo-op/1 infinite? 'real)
  flinfinite? 
  (make-typo-op/1 infinite? 'real))

(define-binary min/2 contagion/will
  fixnum-min bignum-min ratnum-min (make-typo-op/2 < 'real)
  flmin (make-typo-op/2 min/2 'real))
(define-binary max/2 contagion/will
  fixnum-max bignum-max ratnum-max (make-typo-op/2 < 'real)
  flmax (make-typo-op/2 max/2 'real))

(define (min arg0 . args)
  (reduce (core->integer 0) min/2 (cons arg0 args)))
(define (max arg0 . args)
  (reduce (core->integer 0) max/2 (cons arg0 args)))

(define-binary plus/2 contagion/will
  bignum+ ratnum+ recnum+ fl+ compnum+)
(define-binary minus/2 contagion/will
  bignum- ratnum- recnum- fl- compnum-)
(define-binary //2 contagion/will
  integer/ ratnum/ recnum/ fl/ compnum/)

(define-binary */2-helper contagion/will
  bignum* ratnum* recnum* fl* compnum/)
;; might be done faster with a different contagion matrix
(define (*/2 n1 n2)
  (if (or (and (fixnum? n1)
	       (fixnum=? n1 (core->integer 0)))
	  (and (fixnum? n2)
	       (fixnum=? n2 (core->integer 0))))
      0
      (*/2-helper n1 n2)))

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
  (make-typo-op/1 abs 'real)
  flabs
  (make-typo-op/1 abs 'real))

(define-binary quotient contagion/will
  fixnum-quotient
  bignum-quotient
  (make-typo-op/2 quotient 'integer)
  (make-typo-op/2 quotient 'integer)
  flquotient
  (make-typo-op/2 quotient 'integer))
  
(define-binary remainder contagion/will
  fixnum-remainder
  bignum-remainder
  (make-typo-op/2 remainder 'integer)
  (make-typo-op/2 remainder 'integer)
  flremainder
  (make-typo-op/2 remainder 'integer))

(define-binary quotient+remainder contagion/will
  fixnum-quotient+remainder
  bignum-quotient+remainder
  (make-typo-op/2 quotient+remainder 'integer)
  (make-typo-op/2 quotient+remainder 'integer)
  flquotient+remainder
  (make-typo-op/2 quotient+remainder 'integer))

; from Scheme 48

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
		  (- (quotient (- (- d n) (core->integer 1)) d))
		  (quotient n d))))
	   ((zero? y)
	    (core->integer 0))
	   ((negative? y)
	    (let ((n (* (core->integer -2)
			(numerator x)
			(denominator y)))
		  (d (* (denominator x)
			(- (numerator y)))))
	      (if (< n d)
		  (- (quotient (- d n) (* (core->integer 2) d)))
		  (quotient (+ n d (core->integer -1)) (* (core->integer 2) d)))))))
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

; end from Scheme 48

(define (flnumerator x)
  (integer->flonum (numerator (flonum->rational x))))
(define (fldenominator x)
  (integer->flonum (denominator (flonum->rational x))))

(define-unary numerator
  id id ratnum-numerator
  (make-typo-op/1 numerator 'real)
  flnumerator
  (make-typo-op/1 numerator 'real))

(define-unary denominator
  one one ratnum-denominator
  (make-typo-op/1 denominator 'real)
  fldenominator
  (make-typo-op/1 denominator 'real))

;; floor is primitive
(define-unary floor
  id id ratnum-floor
  (make-typo-op/1 floor 'real)
  flfloor
  (make-typo-op/1 floor 'real))

; from Scheme 48

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

; end from Scheme 48

(define-syntax define-unary-flonum/compnum
  (syntax-rules ()
    ((define-unary-flonum/compnum ?name ?flonum-op ?compnum-op)
     (define (?name z)
       (cond
	((flonum? z) (?flonum-op z))
	((compnum? z) (?compnum-op z))
	((recnum? z)
	 (?compnum-op (recnum->compnum z)))
	((ratnum? z)
	 (?flonum-op (ratnum->flonum z)))
	((bignum? z)
	 (?flonum-op (bignum->flonum z)))
	((fixnum? z)
	 (?flonum-op (fixnum->flonum z))))))))

(define-unary-flonum/compnum exp flexp compnum-exp)
(define-unary-flonum/compnum sin flsin compnum-sin)
(define-unary-flonum/compnum cos flcos compnum-cos)
(define-unary-flonum/compnum tan fltan compnum-tan)
(define-unary-flonum/compnum asin flasin compnum-asin)
(define-unary-flonum/compnum acos flacos compnum-acos)
(define-unary-flonum/compnum atan1 flatan compnum-atan1)

; from Larceny

(define (log1 z)
  (cond ((and (flonum? z) (flpositive? z))
	 (fllog z))
	((or (not (real? z)) (negative? z))
	 (+ (log (magnitude z)) (* (core->compnum +1.0i) (angle z))))
	((and (exact? z) (zero? z))
	 (error "log: Domain error: " z)
	 #t)
	(else
	 (fllog (x->inexact z)))))

(define (log z . extra)
  (if (null? extra)
      (log1 z)
      (/ (log1 z)
	 (log1 (car extra)))))

; Square root
; Formula for complex square root from CLtL2, p310.

(define (sqrt z)
  (cond ((and (flonum? z) (not (flnegative? z)))
	 (flsqrt z))
	((not (real? z))
	 (exp (/ (log z) (core->flonum 2.0))))
	((negative? z)
	 (make-rectangular (core->integer 0) (sqrt (- z))))
	(else
	 (flsqrt (x->inexact z)))))

(define (atan z . rest)
  (if (null? rest)
      (atan1 z)
      (let ((x z)
	    (y (car rest)))
	(cond ((and (flonum? x) (flonum? y))
	       (flatan x y))
	      ((not (and (real? x) (real? y)))
	       (error "ATAN: domain error" x y)
	       #t)
	      (else
	       (flatan (x->inexact x) (x->inexact y)))))))

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
	     (if (and (exact? x) (exact? y))
		 (core->integer 1)
		 (core->flonum 1.0))
	     (if (exact? x)
		 (core->integer 0)
		 (core->flonum 0.0))))
	((integer? y)
	 (if (negative? y)
	     (/ (expt x (- y)))
	     (e x y)))
	(else
	 (exp (* y (log x))))))

; end from Larceny

(define (exact-rational? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)))

(define (non-real? obj)
  (or (recnum? obj)
      (compnum? obj)))

(define (make-rectangular a b)

  (define (fail)
    (error "make-rectangular: non-real argument" a b))

  (cond
   ((flonum? a)
    (cond
     ((flonum? b)
      (make-compnum a b))
     ((compnum? b)
      (fail))
     ((and (exact? b)
	   (zero? b))
      a)
     (else
      (make-rectangular a (x->inexact b)))))
   ((exact-rational? a)
    (cond
     ((exact-rational? b)
      (rectangulate a b))
     ((flonum? b)
      (make-rectangular (x->inexact a) b))
     (else
      (fail))))
   (else
    (fail))))

; from Larceny

(define (make-polar a b)
  (if (not (and (real? a) (real? b)))
      (begin
	(error "make-polar: invalid arguments" a b)
	#t)
      (make-rectangular (* a (cos b)) (* a (sin b)))))

(define (real-part z)
  (cond
   ((recnum? z) (recnum-real z))
   ((compnum? z) (compnum-real z))
   (else
    z)))

(define (imag-part z)
  (cond
   ((recnum? z) (recnum-imag z))
   ((compnum? z) (compnum-imag z))
   (else (core->integer 0))))

(define (angle c)
  (atan (imag-part c) (real-part c)))

; NOTE: CLtL2 notes that this implementation may not be ideal for very
;       large or very small numbers.

(define (magnitude c)
  (let ((r (real-part c))
	(i (imag-part c)))
    (if (or (= r flinf+)
	    (= r flinf-)
	    (= i flinf+)
	    (= i flinf-))
	flinf+
	(sqrt (+ (* r r) (* i i))))))

; end from Larceny

(define-unary x->inexact
  fixnum->flonum bignum->flonum ratnum->flonum recnum->compnum id id)

(define (compnum->exact a)
  (make-rectangular (x->exact (real-part a))
		    (x->exact (imag-part a))))
(define-unary x->exact
  id id id id flonum->rational compnum->exact)

(define-unary number->flonum
  fixnum->flonum bignum->flonum ratnum->flonum
  (make-typo-op/1 number->flonum 'real)
  id
  maybe-compnum->flonum)

(define (maybe-compnum->flonum c)
  (if (flzero? (compnum-imag c))
      (compnum-real c)
      (error "can't convert complex number with non-zero imaginary part to flonum" c)))

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
         (cond ((real? x) x)
	       ((real? y) y)
	       (else (error "(rationalize <non-real> 0)" x))))
        ((positive? x)
         (simplest-rational-internal x y))
        ((negative? y)
         (- (core->integer 0)
	    (simplest-rational-internal (- (core->integer 0) y)
                                        (- (core->integer 0) x))))
        (else
         (if (and (exact? x) (exact? y))
             (core->integer 0)
             (x->inexact (core->integer 0))))))
)
