; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic inexact arithmetic

(define (inexact-number? obj)
  (or (flonum? obj)
      (compnum? obj)))

(define (inexact-complex? obj)
  (inexact-number? obj))

(define (inexact-real? obj)
  (flonum? obj))

(define (inexact-rational? obj)
  (and (inexact-real? obj)
       (not (or (inexact=? obj flinf+)
		(inexact=? obj flinf-)
		(flnan? obj)))))

(define (inexact-integer? obj)
  (and (flonum? obj)
       (flinteger? obj)))

(define-syntax define-binary
  (syntax-rules ()
    ((define-binary ?name ?contagion ?flonum-op ?compnum-op)
     (define (?name a b)
       (cond
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

(define-binary inexact=?/2 econtagion/in
  fl=? compnum=?)

(define-binary inexact<?/2 pcontagion/in
  fl<? (make-typo-op/2 inexact<? 'real))
(define-binary inexact<=?/2 pcontagion/in
  fl<=? (make-typo-op/2 inexact<=? 'real))
(define-binary inexact>=?/2 pcontagion/in
  fl>=? (make-typo-op/2 inexact>=? 'real))
(define-binary inexact>?/2 pcontagion/in
  fl>? (make-typo-op/2 inexact>? 'real))

(define inexact=? (make-transitive-pred inexact=?/2))
(define inexact<? (make-transitive-pred inexact<?/2))
(define inexact<=? (make-transitive-pred inexact<=?/2))
(define inexact>=? (make-transitive-pred inexact>=?/2))
(define inexact>? (make-transitive-pred inexact>=?/2))

(define-syntax define-unary
  (syntax-rules ()
    ((define-unary ?name ?flonum-op ?compnum-op)
     (define (?name a)
       (cond
	((flonum? a)
	 (?flonum-op a))
	((compnum? a)
	 (?compnum-op a))
	(else
	 (error "expects an inexact argument" ?name a)))))))

(define-unary inexact-zero? flzero? compnum-zero?) ; #### compnum case?
(define-unary inexact-positive? flpositive?
  (make-typo-op/1 inexact-positive? 'real))
(define-unary inexact-negative? flnegative?
  (make-typo-op/1 inexact-negative? 'real))
(define-unary inexact-odd? flodd?
  (make-typo-op/1 inexact-odd? 'real))
(define-unary inexact-even? fleven?
  (make-typo-op/1 inexact-even? 'real))
(define-unary inexact-nan? flnan? 
  (make-typo-op/1 inexact-nan? 'real))
(define-unary inexact-finite? flnan? 
  (make-typo-op/1 inexact-finite? 'real))
(define-unary inexact-infinite? flnan? 
  (make-typo-op/1 inexact-infinite? 'real))

(define inexact-min (make-min/max inexact<?))
(define inexact-max (make-min/max inexact>?))

(define-binary inexact+/2 contagion/in fl+ compnum+)
(define-binary inexact-/2 contagion/in fl- compnum-)
(define-binary inexact*/2 contagion/in fl* compnum*)
(define-binary inexact//2 contagion/in fl/ compnum/)

(define (inexact+ . args)
  (reduce (core->flonum 0.0) inexact+/2 args))
(define (inexact- arg0 . args)
  (reduce (core->flonum 0.0) inexact-/2 (cons arg0 args)))
(define (inexact* . args)
  (reduce (core->flonum 1.0) inexact*/2 args))
(define (inexact/ arg0 . args)
  (reduce (core->flonum 1.0) inexact//2 (cons arg0 args)))

(define-unary inexact-abs flabs (make-typo-op/1 inexact-abs 'real))

(define-binary inexact-quotient contagion/in
  flquotient
  (make-typo-op/2 inexact-quotient 'real))
  
(define-binary inexact-remainder contagion/in
  flremainder
  (make-typo-op/2 inexact-remainder 'real))

(define-binary inexact-quotient+remainder contagion/in
  inexact-quotient+remainder
  (make-typo-op/2 inexact-quotient+remainder 'real))

; from Scheme 48

(define (inexact-modulo x y)
  (if (and (inexact-integer? x) (inexact-integer? y))
      (let* ((q (inexact-quotient x y))
	     (r (inexact- x (inexact* q y))))
	(cond ((inexact-zero? r)
	       r)
	      ((inexact-negative? r)
	       (if (inexact-negative? y)
		   r
		   (inexact+ r y)))
	      ((inexact-negative? y)
	       (inexact+ r y))
	      (else
	       r)))
      (error "inexact-modulo expects integral arguments" x y)))

; from "Cleaning up the Tower"

(define (inexact-div+mod x y)
  (let* ((div
	  (cond
	   ((inexact-positive? y)
	    (let ((n (inexact* (inexact-numerator x)
			       (inexact-denominator y)))
		  (d (inexact* (inexact-denominator x)
			       (inexact-numerator y))))
	      (if (inexact-negative? n)
		  (inexact- (inexact-quotient (inexact- (inexact- d n) (core->flonum 1)) d))
		  (inexact-quotient n d))))
	   ((inexact-zero? y)
	    (core->flonum 0))
	   ((inexact-negative? y)
	    (let ((n (inexact* (core->flonum -2) 
			       (inexact-numerator x)
			       (inexact-denominator y)))
		  (d (inexact* (inexact-denominator x)
			  (inexact- (inexact-numerator y)))))
	      (if (inexact<? n d)
		  (inexact- (inexact-quotient (inexact- d n) (inexact* 2 d)))
		  (inexact-quotient (inexact+ n d (core->flonum -1)) (inexact* 2 d)))))))
	 (mod
	  (inexact- x (inexact* div y))))
    (values div mod)))

(define (inexact-div x y)
  (call-with-values
      (lambda () (inexact-div+mod x y))
    (lambda (d m)
      d)))

(define (inexact-mod x y)
  (call-with-values
      (lambda () (inexact-div+mod x y))
    (lambda (d m)
      m)))

(define (inexact-gcd/2 x y)
  (if (and (inexact-integer? x) (inexact-integer? y))
      (cond ((inexact<? x (core->flonum 0.0)) (inexact-gcd/2 (inexact- x) y))
	    ((inexact<? y (core->flonum 0.0)) (inexact-gcd/2 x (inexact- y)))
	    ((inexact<? x y) (euclid y x))
	    (else (euclid x y)))
      (error "ingcd inpects integral arguments" x y)))

(define (euclid x y)
  (if (inexact-zero? y)
      x
      (euclid y (inexact-remainder x y))))

(define (inexact-lcm/2 x y)
  (let ((g (inexact-gcd/2 x y)))
    (if (inexact-zero? g)
	g
	(inexact* (inexact-quotient (inexact-abs x) g)
		  (inexact-abs y)))))

(define (inexact-gcd . args)
  (reduce (core->flonum 0.0) inexact-gcd/2 args))

(define (inexact-lcm . args)
  (reduce (core->flonum 1.0) inexact-lcm/2 args))

(define (flnumerator x)
  (integer->flonum (rational-numerator (flonum->rational x))))
(define (fldenominator x)
  (integer->flonum (rational-denominator (flonum->rational x))))

(define-unary inexact-numerator
  flnumerator
  (make-typo-op/1 inexact-numerator 'real))

(define-unary inexact-denominator
  fldenominator
  (make-typo-op/1 inexact-denominator 'real))

;; floor is primitive
(define-unary inexact-floor
  flfloor
  (make-typo-op/1 inexact-floor 'real))

(define (inexact-ceiling x)
  (inexact- (inexact-floor (inexact- x))))

(define (inexact-truncate x)
  (if (inexact-negative? x)
      (inexact-ceiling x)
      (inexact-floor x)))

(define (inexact-round x)
  (let* ((x+1/2 (inexact+ x (core->flonum 0.5)))
	 (r (inexact-floor x+1/2)))
    (if (and (inexact=? r x+1/2)
	     (inexact-odd? r))
	(inexact- r (core->flonum 1.0))
	r)))

(define-unary inexact-exp flexp compnum-exp)
(define-unary inexact-sin flsin compnum-sin)
(define-unary inexact-cos flcos compnum-cos)
(define-unary inexact-tan fltan compnum-tan)
(define-unary inexact-asin flasin compnum-asin)
(define-unary inexact-acos flacos compnum-acos)
(define-unary inexact-atan1 flatan compnum-atan1)

; from Larceny

(define (inexact-log1 z)
  (cond ((and (flonum? z) (flpositive? z))
	 (fllog z))
	((or (compnum? z) (inexact-negative? z))
	 (inexact+ (inexact-log (inexact-magnitude z))
		   (inexact* (core->compnum +1.0i) (inexact-angle z))))
	(else
	 (fllog z))))

(define (inexact-log z . extra)
  (if (null? extra)
      (inexact-log1 z)
      (inexact/ (inexact-log1 z)
		(inexact-log1 (car extra)))))

; Square root
; Formula for complex square root from CLtL2, p310.

(define (inexact-sqrt z)
  (cond ((and (flonum? z) (not (flnegative? z)))
	 (flsqrt z))
	((compnum? z)
	 (inexact-exp (inexact/ (inexact-log z) (core->flonum 2.0))))
	((inexact-negative? z)
	 (inexact-make-rectangular (core->flonum 0.0) (inexact-sqrt (inexact- z))))
	(else
	 (flsqrt z))))

(define (inexact-atan z . rest)
  (if (null? rest)
      (inexact-atan1 z)
      (let ((x z)
	    (y (car rest)))
	(cond ((and (flonum? x) (flonum? y))
	       (flatan x y))
	      ((not (and (flonum? x) (flonum? y)))
	       (error "ATAN: domain error: " x y)
	       #t)
	      (else
	       (flatan x y))))))

(define (inexact-expt x y)

  (define (e x y)
    (cond ((inexact-zero? y)
	   (core->flonum 1.0))
	  ((inexact-odd? y)
	   (inexact* x (e x (inexact- y (core->flonum 1.0)))))
	  (else 
	   (let ((v (e x (inexact-quotient y (core->flonum 2.0)))))
	     (inexact* v v)))))

  (cond ((inexact-zero? x)
	 (if (inexact-zero? y)
	     (core->flonum 1.0)
	     (core->flonum 0.0)))
	((inexact-integer? y)
	 (if (inexact-negative? y)
	     (inexact/ (inexact-expt x (inexact- y)))
	     (e x y)))
	(else
	 (inexact-exp (inexact* y (inexact-log x))))))


(define (inexact-make-rectangular a b)
  (if (and (flonum? a)
	   (flonum? b))
      (make-compnum a b)
      (error "inexact-make-rectangular: non-real argument" a b)))

; from Larceny

(define (inexact-make-polar a b)
  (if (not (and (flonum? a) (flonum? b)))
      (begin
	(error "make-polar: invalid arguments" a b)
	#t)
      (inexact-make-rectangular (inexact* a (inexact-cos b))
				(inexact* a (inexact-sin b)))))

(define (inexact-real-part z)
  (cond
   ((compnum? z) (compnum-real z))
   ((flonum? z) z)
   (else
    (error "inexact-real-part: invalid argument" z))))

(define (inexact-imag-part z)
  (cond
   ((compnum? z) (compnum-real z))
   ((flonum? z) (core->flonum 0.0))
   (else
    (error "inexact-imag-part: invalid argument" z))))

(define (inexact-angle c)
  (inexact-atan (inexact-imag-part c) (inexact-real-part c)))

; NOTE: CLtL2 notes that this implementation may not be ideal for very
;       large or very small numbers.

(define (inexact-magnitude c)
  (let ((r (inexact-real-part c))
	(i (inexact-imag-part c)))
    (inexact-sqrt (inexact+ (inexact* r r) (inexact* i i)))))

; end from Larceny


