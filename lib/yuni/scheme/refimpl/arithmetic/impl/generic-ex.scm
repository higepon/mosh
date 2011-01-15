(library (yuni scheme refimpl arithmetic impl generic-ex)
         (export
           (rename
             (exact-div+mod div-and-mod)
             (exact-div0+mod0 div0-and-mod0)
             (exact-div0 div0)
             (exact-mod0 mod0)
             (exact-sqrt exact-integer-sqrt)
             (exact-bit-field bitwise-bit-field)
             (exact-length bitwise-length)
             (exact-not bitwise-not)
             (exact-ior bitwise-ior)
             (exact-and bitwise-and)
             (exact-xor bitwise-xor)
             (exact-if bitwise-if)
             (exact-bit-count bitwise-bit-count)
             (exact-first-bit-set bitwise-first-bit-set)
             (exact-bit-set? bitwise-bit-set?)
             (exact-copy-bit bitwise-copy-bit)
             (exact-copy-bit-field bitwise-copy-bit-field)
             (exact-arithmetic-shift bitwise-arithmetic-shift)
             (exact-arithmetic-shift-left bitwise-arithmetic-shift-left)
             (exact-arithmetic-shift-right bitwise-arithmetic-shift-right)
             (exact-rotate-bit-field bitwise-rotate-bit-field)
             (exact-reverse-bit-field bitwise-reverse-bit-field)) )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl arithmetic-util)
                 (yuni scheme refimpl arithmetic impl contagion-ex)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl bigbit)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 (yuni scheme refimpl arithmetic impl recnum))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Generic exact rational arithmetic

(define (exact-number? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)
      (recnum? obj)))

(define (exact-complex? obj)
  (exact-number? obj))

(define (exact-rational? obj)
  (or (fixnum? obj)
      (bignum? obj)
      (ratnum? obj)))

#| dupe?
(define (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))
|#

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

(define-binary exact=?/2 econtagion/ex
  fixnum=? bignum=? ratnum=? recnum=?)

(define-binary exact<?/2 pcontagion/ex
  fixnum<? bignum<? ratnum<? (make-typo-op/2 exact<? 'rational))
(define-binary exact<=?/2 pcontagion/ex
  fixnum<? bignum<=? ratnum<=? (make-typo-op/2 exact<=? 'rational))
(define-binary exact>=?/2 pcontagion/ex
  fixnum>=? bignum>=? ratnum>=? (make-typo-op/2 exact>=? 'rational))
(define-binary exact>?/2 pcontagion/ex
  fixnum>=? bignum>? ratnum>? (make-typo-op/2 exact>? 'rational))

(define exact=? (make-transitive-pred exact=?/2))
(define exact<? (make-transitive-pred exact<?/2))
(define exact<=? (make-transitive-pred exact<=?/2))
(define exact>=? (make-transitive-pred exact>=?/2))
(define exact>? (make-transitive-pred exact>=?/2))

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
	 (error "expects an exact argument" ?name a)))))))

(define-unary exact-zero? fixnum-zero? bignum-zero? never never) 
(define-unary exact-positive? fixnum-positive? bignum-positive? ratnum-positive?
  (make-typo-op/1 exact-positive? 'rational))
(define-unary exact-negative? fixnum-negative? bignum-negative? ratnum-negative?
  (make-typo-op/1 exact-negative? 'rational))
(define-unary exact-odd? fixnum-odd? bignum-odd?
  (make-typo-op/1 exact-odd? 'integer)
  (make-typo-op/1 exact-odd? 'integer))
(define-unary exact-even? fixnum-even? bignum-even?
  (make-typo-op/1 exact-even? 'integer)
  (make-typo-op/1 exact-even? 'integer))

(define exact-min (make-min/max exact<?))
(define exact-max (make-min/max exact>?))

(define-binary exact+/2 contagion/ex
  bignum+ ratnum+ recnum+)
(define-binary exact-/2 contagion/ex
  bignum- ratnum- recnum-)
(define-binary exact*/2 contagion/ex
  bignum* ratnum* recnum*)
(define-binary exact//2 contagion/ex
  integer/ integer/ ratnum/ recnum/)

(define (exact+ . args)
  (reduce (core->integer 0) exact+/2 args))

(define (exact- arg0 . args)
  (if (null? args)
      (exact-/2 (core->integer 0) arg0)
      ;FIXME: the definition of reduce in nary.scm is bonkers
      (do ((result arg0 (exact-/2 result (car args)))
           (args args (cdr args)))
          ((null? args) result))))

(define (exact* . args)
  (reduce (core->integer 1) exact*/2 args))
(define (exact/ arg0 . args)
  (if (null? args)
      (exact//2 (core->integer 1) arg0)
      ;FIXME: the definition of reduce in nary.scm is bonkers
      (do ((result arg0 (exact//2 result (car args)))
           (args args (cdr args)))
          ((null? args) result))))

;; ABS is evil ...
(define *minus-least-fixnum* (bignum-negate (fixnum->bignum (least-fixnum))))

(define (fixnum-abs x)
  (cond
   ((fixnum-negative? x)
    (if (fixnum=? x (least-fixnum))
        *minus-least-fixnum*
	(fixnum- x)))
   (else x)))

(define-unary exact-abs
  fixnum-abs bignum-abs ratnum-abs
  (make-typo-op/1 exact-abs 'rational))

(define-binary exact-quotient contagion/ex
  (lambda (x y)
    (if (and (fixnum=? x (least-fixnum))
             (fixnum=? y (core->integer -1)))
        *minus-least-fixnum*
        (fixnum-quotient x y)))
  bignum-quotient
  (make-typo-op/2 exact-quotient 'integer)
  (make-typo-op/2 exact-quotient 'integer))
  
(define-binary exact-remainder contagion/ex
  fixnum-remainder
  bignum-remainder
  (make-typo-op/2 exact-remainder 'integer)
  (make-typo-op/2 exact-remainder 'integer))

(define-binary exact-quotient+remainder contagion/ex
  fixnum-quotient+remainder
  bignum-quotient+remainder
  (make-typo-op/2 exact-quotient+remainder 'integer)
  (make-typo-op/2 exact-quotient+remainder 'integer))

(define (exact-modulo x y)
  (if (and (exact-integer? x) (exact-integer? y))
      (let* ((q (exact-quotient x y))
	     (r (exact- x (exact* q y))))
	(cond ((exact-zero? r)
	       r)
	      ((exact-negative? r)
	       (if (exact-negative? y)
		   r
		   (exact+ r y)))
	      ((exact-negative? y)
	       (exact+ r y))
	      (else
	       r)))
      (error "exact-modulo expects integral arguments" x y)))

(define (exact-div+mod x y)
  (cond ((or (not (exact-number? x))
             (not (exact-number? y)))
         (error "illegal arguments to exact-div+mod" x y))
        ((exact-zero? y)
         (error "exact division by zero" x y))
        (else
         ; FIXME: this isn't very efficient
         (let* ((n (exact* (exact-numerator x)
                           (exact-denominator y)))
                (d (exact* (exact-denominator x)
                           (exact-numerator y)))
                (q (exact-quotient n d))
                (m (exact- x (exact* q y))))
           ; x = xn/xd
           ; y = yn/yd
           ; n = xn*yd
           ; d = xd*yn
           ; x/y = (xn*yd)/(xd*yn) = n/d
           (if (exact-negative? m)
               (if (exact-positive? y)
                   (values (exact- q (core->integer 1))
                           (exact+ m y))
                   (values (exact+ q (core->integer 1))
                           (exact- m y)))                   
               (values q m))))))

(define (exact-div x y)
  (call-with-values
      (lambda () (exact-div+mod x y))
    (lambda (d m)
      d)))

(define (exact-mod x y)
  (call-with-values
      (lambda () (exact-div+mod x y))
    (lambda (d m)
      m)))

(define (exact-div0+mod0 x y)
  (call-with-values
   (lambda () (exact-div+mod x y))
   (lambda (d m)
     (let ((y/2 (exact/ y (core->integer 2))))
       (cond ((exact-positive? y/2)
              (if (exact>=? m y/2)
                  (values (exact+ d (core->integer 1))
                          (exact- m y))
                  (values d m)))
             (else
              (let ((y/2abs (exact- y/2)))
                (if (exact>=? m y/2abs)
                    (values (exact- d (core->integer 1))
                            (exact+ m y))
                    (values d m)))))))))

(define (exact-div0 x y)
  (call-with-values
   (lambda () (exact-div0+mod0 x y))
   (lambda (d m) d)))

(define (exact-mod0 x y)
  (call-with-values
   (lambda () (exact-div0+mod0 x y))
   (lambda (d m) m)))

(define (exact-gcd/2 x y)
  (if (and (exact-integer? x) (exact-integer? y))
      (cond ((exact<? x (core->integer 0)) (exact-gcd/2 (exact- x) y))
	    ((exact<? y (core->integer 0)) (exact-gcd/2 x (exact- y)))
	    ((exact<? x y) (euclid y x))
	    (else (euclid x y)))
      (error "exgcd expects integral arguments" x y)))

(define (euclid x y)
  (if (exact-zero? y)
      x
      (euclid y (exact-remainder x y))))

(define (exact-lcm/2 x y)
  (let ((g (exact-gcd/2 x y)))
    (if (exact-zero? g)
	g
	(exact* (exact-quotient (exact-abs x) g)
		(exact-abs y)))))

(define (exact-gcd . args)
  (reduce (core->integer 0) exact-gcd/2 args))

(define (exact-lcm . args)
  (reduce (core->integer 1) exact-lcm/2 args))

(define-unary exact-numerator
  id id ratnum-numerator
  (make-typo-op/1 exact-numerator 'rational))

(define-unary exact-denominator
  one one ratnum-denominator
  (make-typo-op/1 exact-denominator 'rational))

;; floor is primitive
(define-unary exact-floor
  id id ratnum-floor
  (make-typo-op/1 exact-floor 'rational))

(define (exact-ceiling x)
  (exact- (exact-floor (exact- x))))

(define (exact-truncate x)
  (if (exact-negative? x)
      (exact-ceiling x)
      (exact-floor x)))

(define (exact-round x)
  (let* ((x+1/2 (exact+ x (core->ratnum 1/2)))
	 (r (exact-floor x+1/2)))
    (if (and (exact=? r x+1/2)
	     (exact-odd? r))
	(exact- r (core->integer 1))
	r)))

(define (exact-expt x y)

  (define (e x y)
    (cond ((exact-zero? y)
	   (core->integer 1))
	  ((exact-odd? y)
	   (exact* x (e x (exact- y (core->integer 1)))))
	  (else 
	   (let ((v (e x (exact-quotient y (core->integer 2)))))
	     (exact* v v)))))

  (cond ((exact-zero? x)
	 (if (exact-zero? y)
	     (core->integer 1)
	     (core->integer 0)))
	((exact-integer? y)
	 (if (exact-negative? y)
	     (exact/ (exact-expt x (exact- y)))
	     (e x y)))
	(else
	 (error "exact-expt expects integer power" y))))

(define (exact-make-rectangular a b)
  (if (and (exact-rational? a)
	   (exact-rational? b))
      (rectangulate a b)
      (error "exact-make-rectangular: non-rational argument" a b)))

(define-unary exact-real-part id id id recnum-real) 
(define-unary exact-imag-part
  (lambda (x) (core->integer 0))  ; don't try to call this zero
  (lambda (x) (core->integer 0))  ; because some other file has
  (lambda (x) (core->integer 0))  ; a global variable named zero
  recnum-imag)

; from Brad Lucier:

(define (exact-sqrt x)

  (if (exact-negative? x)
      (error "exact-sqrt: negative argument" x))

  ;; x is non-negative.  Returns (values s r) where
  ;; x = s^2+r, x < (s+1)^2

  ;; Derived from the paper "Karatsuba Square Root" by Paul Zimmermann,
  ;; INRIA technical report RR-3805, 1999.  (Used in gmp 4.*)

  ;; Note that the statement of the theorem requires that
  ;; b/4 <= high-order digit of x < b which can be impossible when b is a
  ;; power of 2; the paper later notes that it is the lower bound that is 
  ;; necessary, which we preserve.

  (if (and (fixnum? x)
           ;; we require that
           ;; (< (flsqrt (- (* y y) 1)) y) => #t
           ;; whenever x=y^2 is in this range.  Here we assume that we
	   ;; have at least as much precision as IEEE double precision and 
           ;; we round to nearest.
	   (exact<=? x (core->integer 4503599627370496))) ; 2^52
      (let* ((s (flonum->fixnum (flsqrt (fixnum->flonum x)))) 

             (r (fixnum- x (fixnum* s s))))
        (values s r))
      (let ((length/4
             (fixnum-arithmetic-shift-right
              (fixnum+ (exact-integer-length x) (core->integer 1))
              (core->integer 2))))
	(call-with-values
	    (lambda ()
	      (exact-sqrt
	       (exact-arithmetic-shift-left
		x
		(fixnum- (fixnum-arithmetic-shift-left length/4 (core->integer 1))))))
	  (lambda (s-prime r-prime)
	    (call-with-values
		(lambda ()
		  (exact-div+mod
		   (exact+ (exact-arithmetic-shift-left r-prime length/4)
			   (extract-bit-field length/4 length/4 x))
		   (exact-arithmetic-shift-left s-prime (core->integer 1))))
	      (lambda (q u)
		(let ((s
		       (exact+ (exact-arithmetic-shift-left s-prime length/4) q))
		      (r
		       (exact- (exact+ (exact-arithmetic-shift-left u length/4)
				       (extract-bit-field length/4 (core->integer 0) x))
			       (exact* q q))))
		  (if (exact-negative? r)
		      (values  (exact- s (core->integer 1))
			       (exact+ r
				       (exact- (exact-arithmetic-shift-left s (core->integer 1))
					       (core->integer 1))))
		      (values s r))))))))))

;; helper for EXACT-INTEGER-SQRT
;; extract bits of n3, at index n2 (from the right), n1 bits wide

(define (extract-bit-field n1 n2 n3)
  (exact-and (exact-arithmetic-shift-left n3 (exact- n2))
	     (exact- (exact-arithmetic-shift-left (core->integer 1) n1) 
                     (core->integer 1))))


; Integer-length, a la Common Lisp, written in portable Scheme.

; from Scheme 48

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))
(define head car)
(define (tail s) (force (cdr s)))

(define exact-integer-length
  (let ()
    (define useful
      (let loop ((p (exact-expt (core->integer 2) (core->integer 8))) (n (core->integer 4)))
	(cons-stream (cons p n)
		     (loop (exact* p p) (exact* n (core->integer 2))))))
    
    (define upto-16
      (vector (core->integer 0) (core->integer 1) 
	      (core->integer 2) (core->integer 2)
	      (core->integer 3) (core->integer 3) (core->integer 3) (core->integer 3)
	      (core->integer 4) (core->integer 4) (core->integer 4) (core->integer 4)
	      (core->integer 4) (core->integer 4) (core->integer 4) (core->integer 4)))
    
    (define (recur n)
      (if (exact<? n (core->integer 16))
	  (vector-ref upto-16 (integer->core n))
	  (let loop ((s useful) (prev (core->integer 16)))
	    (let ((z (head s)))
	      (if (exact<? n (car z))
		  (exact+ (cdr z) (recur (exact-quotient n prev)))
		  (loop (tail s) (car z)))))))
    (define (integer-length n)
      (if (exact<? n (core->integer 0))
	  (recur (exact- (core->integer -1) n))
	  (recur n)))

    integer-length))

; end from Scheme 48

(define-unary exact-not fixnum-not bignum-not
  (make-typo-op/1 exact-not 'exact-integer)
  (make-typo-op/1 exact-not 'exact-integer))

(define (make-binary-bitwise-op fix-op big-op)

  (lambda (a b)
    (define (fail)
      (error "bitwise operation expects exact integer arguments" a b))

    (cond
     ((fixnum? a)
      (cond
       ((fixnum? b)
	(fix-op a b))
       ((bignum? b)
	(big-op (fixnum->bignum a) b))
       (else (fail))))
     ((bignum? a)
      (cond
       ((fixnum? b)
	(big-op a (fixnum->bignum b)))
       ((bignum? b)
	(big-op a b))
       (else (fail))))
     (else
      (fail)))))

(define (exact-ior . args)
  (reduce (core->integer 0)
	  (make-binary-bitwise-op fixnum-ior bignum-ior)
	  args))

(define (exact-and . args)
  (reduce (core->integer -1)
	  (make-binary-bitwise-op fixnum-and bignum-and)
	  args))

(define (exact-xor . args)
  (reduce (core->integer 0)
	  (make-binary-bitwise-op fixnum-xor bignum-xor)
	  args))

(define (exact-if ei1 ei2 ei3)
  (exact-ior (exact-and ei1 ei2)
             (exact-and (exact-not ei1) ei3)))

(define-unary exact-bit-count fixnum-bit-count bignum-bit-count
  (make-typo-op/1 exact-bit-count 'exact-integer)
  (make-typo-op/1 exact-bit-count 'exact-integer))

(define-unary exact-length fixnum-length bignum-length
  (make-typo-op/1 exact-length 'exact-integer)
  (make-typo-op/1 exact-length 'exact-integer))

(define-unary exact-first-bit-set
  fixnum-first-bit-set bignum-first-bit-set
  (make-typo-op/1 exact-first-bit-set 'exact-integer)
  (make-typo-op/1 exact-first-bit-set 'exact-integer))

(define (exact-bit-set? ei1 ei2)
  (cond ((exact-negative? ei2)
         (error "negative second argument to exact-bit-set?" ei2))
        ((and (fixnum? ei1) (fixnum? ei2))
         (fixnum-bit-set? ei1 ei2))
        ((fixnum? ei1)
         #f)
        ((fixnum? ei2)
         ; FIXME: correct but inefficient
         (not (exact-zero?
               (exact-and (exact-arithmetic-shift-left (core->integer 1) ei2)
                          ei1))))
        (else
         #f)))

(define (exact-copy-bit ei1 ei2 ei3)
  (let* ((mask (exact-arithmetic-shift-left (core->integer 1) ei2)))
    (exact-if mask
              (exact-arithmetic-shift-left ei3 ei2)
              ei1)))

(define (exact-bit-field ei1 ei2 ei3)
  (let* ((mask (exact-not
                (exact-arithmetic-shift-left (core->integer -1) ei3))))
    (exact-arithmetic-shift-right (exact-and ei1 mask)
                                  ei2)))

(define (exact-copy-bit-field to start end from)
  (let* ((mask1 (exact-arithmetic-shift-left (core->integer -1) start))
         (mask2 (exact-not
                 (exact-arithmetic-shift-left (core->integer -1) end)))
         (mask (exact-and mask1 mask2)))
    (exact-if mask
              (exact-arithmetic-shift-left from start)
              to)))

(define (exact-arithmetic-shift ei1 ei2)
  (if (exact>=? ei2 (core->integer 0))
      (exact-arithmetic-shift-left ei1 ei2)
      (exact-arithmetic-shift-right ei1 (exact- ei2))))

(define (exact-arithmetic-shift-left a b)

  (define (fail)
    (error "exact-arithmetic-shift-left expects exact integer arguments" a b))

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

(define (exact-arithmetic-shift-right a b)

  (define (fail)
    (error "exact-arithmetic-shift-right expects exact integer arguments" a b))

  (cond ((fixnum? a)
         (cond ((fixnum? b)
                (bignum-arithmetic-shift-right (fixnum->bignum a)
                                               (fixnum->bignum b)))
               ((bignum? b)
                (bignum-arithmetic-shift-right (fixnum->bignum a) b))
               (else (fail))))
        ((bignum? a)
         (cond ((fixnum? b)
                (bignum-arithmetic-shift-right a (fixnum->bignum b)))
               ((bignum? b)
                (bignum-arithmetic-shift-right a (fixnum->bignum b)))
               (else (fail))))))

(define (exact-rotate-bit-field n start end count)
  (let ((width (exact- end start)))
    (if (exact-positive? width)
        (let* ((count (exact-mod count width))
               (field0 (exact-bit-field n start end))
               (field1 (exact-arithmetic-shift-left field0 count))
               (field2 (exact-arithmetic-shift-right field0
                                                     (exact- width count)))
               (field (exact-ior field1 field2)))
          (exact-copy-bit-field n start end field))
        n)))

(define (exact-reverse-bit-field n start end)
  (if (or (exact-negative? start)
          (exact-negative? end))
      (error "illegal negative argument to exact-reverse-bit-field"
             n start end)
      (let* ((width (exact- end start))
             (field (exact-bit-field n start end)))
        (do ((reversed (core->integer 0)
                       (exact+ (exact+ reversed reversed)
                               (exact-and field (core->integer 1))))
             (field field (exact-arithmetic-shift-right field
                                                        (core->integer 1)))
             (k width (exact- k (core->integer 1))))
            ((exact-zero? k)
             (exact-copy-bit-field n start end reversed))))))
)
