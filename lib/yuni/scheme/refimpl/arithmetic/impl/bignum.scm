(library (yuni scheme refimpl arithmetic impl bignum)
         (export zero-magnitude?
                 log-radix
                 bignum-sign
                 bignum-magnitude
                 bignum?
                 bignum<=?
                 bignum-negate
                 high-digits
                 bignum+
                 adjoin-digit
                 fixnum->magnitude
                 bignum->fixnum
                 fixnum->bignum
                 bignum>?
                 low-digit
                 zero-magnitude
                 bignum<?
                 bignum-
                 make-integer
                 bignum-positive?
                 radix
                 bignum-zero?
                 core->bignum
                 bignum->string
                 bignum=?
                 bignum-quotient+remainder
                 bignum-remainder
                 bignum-quotient
                 bignum*
                 bignum->core
                 bignum-abs
                 bignum-max
                 bignum-min
                 bignum-even?
                 bignum-odd?
                 bignum-negative?
                 bignum>=?
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl custom)
                 (yuni scheme refimpl arithmetic impl bitwise)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 )

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Bignum arithmetic

; FIXME:  The representation of bignums is inefficient,
; and so are the operations.
;
; A bignum is represented as a record containing
;     sign      -- a fixnum, -1 for negative, 1 for non-negative
;     magnitude -- a list of fixnum bigits, least significant first
;
; Each bigit is a non-negative fixnum less than the radix.
; The radix seems to be 2^k, where k = (- (quotient (fixnum-width) 2) 1).

; from Scheme 48

(define* :bignum
  (sign magnitude))

(define (make-bignum x y)
  (make :bignum
        (sign x)
        (magnitude y)))
(define (bignum? x)
  (is-a? x :bignum))

(define (bignum-sign x)
  (let-with x (sign) sign))
(define (bignum-magnitude x)
  (let-with x (magnitude) magnitude))

#|
(define-record-type :bignum
  (make-bignum sign magnitude)
  bignum?
  (sign bignum-sign)
  (magnitude bignum-magnitude))

(define-record-discloser :bignum
  (lambda (r)
    (list 'bignum
	  (bignum->core r))))
|#

(define (fixnum->bignum m)
  (cond ((fixnum>=? m (core->fixnum 0))
	 (make-bignum (core->fixnum 1) (fixnum->magnitude m)))
	((fixnum=? m (least-fixnum))
	 (make-bignum (core->fixnum -1) fixnum-min-magnitude))
	(else
	 (make-bignum (core->fixnum -1) (fixnum->magnitude (fixnum- m))))))

(define (core->bignum m)
  (if (bignum? m)
      m
      (cond ((core:>= m 0)
	     (make-bignum (core->fixnum 1) (core->magnitude m)))
	    ((core:= m (fixnum->core (least-fixnum)))
	     (make-bignum (core->fixnum -1) fixnum-min-magnitude))
	    (else
	     (make-bignum (core->fixnum -1) (core->magnitude (core:- 0 m)))))))

(define (core->magnitude n)
  (let ((radix (fixnum->core radix)))
    (let recur ((n n))
      (if (core:= n 0)
	  zero-magnitude
	  (let ((digit (core->fixnum (core:remainder n radix))))
	    (adjoin-digit digit
			  (recur (core:quotient n radix))))))))

(define (bignum->core n)             ;For debugging
  (core:* (fixnum->core (bignum-sign n))
     (let recur ((digits (bignum-magnitude n)))
       (if (null? digits)
	   0
	   (core:+ (fixnum->core (car digits))
	      (core:* (recur (cdr digits)) (fixnum->core radix)))))))

(define (make-integer sign mag)
  (if (fixnum-positive? sign)
      (if (smaller-magnitude? fixnum-max-magnitude mag)
	  (make-bignum sign mag)
	  (magnitude->integer mag))
      (if (smaller-magnitude? fixnum-min-magnitude mag)
	  (make-bignum sign mag)
	  (if (same-magnitude? mag fixnum-min-magnitude)
	      (least-fixnum)
	      (fixnum- (magnitude->integer mag))))))

(define (bignum->integer m)
  (make-integer (bignum-sign m)
		(bignum-magnitude m)))

; #### assumes symmetric fixnums
(define (bignum->fixnum m)
  (let ((sign (bignum-sign m))
	(mag (bignum-magnitude m)))
    (cond
     ((fixnum-positive? sign)
      (magnitude->integer mag))
     ((same-magnitude? mag fixnum-min-magnitude)
      (least-fixnum))
     (else (fixnum- (magnitude->integer mag))))))

; Arithmetic

(define (bignum+ m n)
  (let ((m-sign (bignum-sign m))
	(m-mag (bignum-magnitude m))
	(n-sign (bignum-sign n))
	(n-mag (bignum-magnitude n)))
    (if (fixnum=? m-sign n-sign)
	(make-integer m-sign (add-magnitudes m-mag n-mag))
	(if (smaller-magnitude? m-mag n-mag)
	    (make-integer (fixnum- m-sign) (subtract-magnitudes n-mag m-mag))
	    (make-integer m-sign (subtract-magnitudes m-mag n-mag))))))

(define (bignum- m n)
  (bignum+ m (bignum-negate n)))

(define (bignum-negate m)
  (make-bignum (fixnum- (bignum-sign m))
	       (bignum-magnitude m)))

(define (bignum* m n)
  (make-integer (fixnum* (bignum-sign m) (bignum-sign n))
		(multiply-magnitudes
		 (bignum-magnitude m)
		 (bignum-magnitude n))))

(define (bignum-divide m n cont)
  (divide-magnitudes
   (bignum-magnitude m)
   (bignum-magnitude n)
   (lambda (q r)
     (cont (make-integer (fixnum* (bignum-sign m) (bignum-sign n)) q)
	   (make-integer (bignum-sign m) r)))))

(define (bignum-quotient m n)
  (bignum-divide m n (lambda (q r) q)))

(define (bignum-remainder m n)
  (bignum-divide m n (lambda (q r) r)))

(define (bignum-quotient+remainder m n)
  (bignum-divide m n values))

(define (bignum=? m n)
  (and (fixnum=? (bignum-sign m) (bignum-sign n))
       (same-magnitude? (bignum-magnitude m)
			(bignum-magnitude n))))

(define (bignum<? m n)
  (let ((m-sign (bignum-sign m))
	(n-sign (bignum-sign n)))
    (or (fixnum<? m-sign n-sign)
	(and (fixnum=? m-sign n-sign)
	     (if (fixnum-negative? m-sign)
		 (smaller-magnitude? (bignum-magnitude n)
				     (bignum-magnitude m))
		 (smaller-magnitude? (bignum-magnitude m)
				     (bignum-magnitude n)))))))


(define (bignum<=? p q)
  (not (bignum<? q p)))

(define (bignum>=? p q)
  (not (bignum<? p q)))

(define (bignum>? p q)
  (bignum<? q p))

(define (bignum-zero? m)
  (zero-magnitude? (bignum-magnitude m)))

(define (bignum-positive? m)
  (fixnum-positive? (bignum-sign m)))
(define (bignum-negative? m)
  (fixnum-negative? (bignum-sign m)))

(define (bignum-odd? m)
  (fixnum-odd? (low-digit (bignum-magnitude m))))
(define (bignum-even? m)
  (fixnum-even? (low-digit (bignum-magnitude m))))

(define (bignum-abs m)
  (if (bignum-negative? m)
      (bignum-negate m)
      m))

(define (bignum-min m n)
  (if (bignum<=? m n)
      (bignum->integer m)
      (bignum->integer n)))

(define (bignum-max m n)
  (if (bignum>=? m n)
      (bignum->integer m)
      (bignum->integer n)))


; Magnitude (unsigned integer) arithmetic

; Fixnum arithmetic without overflow checking sucks
(define log-radix
  (let ((max (fixnum-quotient (greatest-fixnum) (core->fixnum 2)))
	(min (fixnum-quotient (least-fixnum) (core->fixnum 2))))
    (let loop ((l (core->fixnum 1))
	       (r (core->fixnum 1))
	       (rm (core->fixnum -1)))
      (if (or (fixnum>=? r max)
	      (fixnum<=? rm min))
	  (fixnum-quotient l (core->fixnum 2))
	  (loop (fixnum+ (core->fixnum 1) l)
		(fixnum* r(core->fixnum 2)) (fixnum* rm (core->fixnum 2)))))))

(define radix (fixnum-arithmetic-shift-left (core->fixnum 1) log-radix))

(define zero-magnitude '())
(define zero-magnitude? null?)

(define (low-digit m)
  (if (zero-magnitude? m)
      (core->fixnum 0)
      (car m)))

(define (high-digits m)
  (if (zero-magnitude? m)
      m
      (cdr m)))

(define (adjoin-digit d m)
  (if (and (fixnum-zero? d) (zero-magnitude? m))
      m
      (cons d m)))

(define (fixnum->magnitude n)
  (if (fixnum-zero? n)
      zero-magnitude
      (let ((digit (fixnum-remainder n radix)))
	(adjoin-digit digit
		      (fixnum->magnitude (fixnum-quotient n radix))))))

(define (magnitude->integer m)
  (if (zero-magnitude? m)
      (core->fixnum 0)
      (fixnum+ (low-digit m)
               (fixnum* radix (magnitude->integer (high-digits m))))))

(define fixnum-max-magnitude
  (fixnum->magnitude (greatest-fixnum)))

(define fixnum-min-magnitude
  (adjoin-digit (fixnum- (fixnum-remainder (least-fixnum) radix))
		(fixnum->magnitude
		 (fixnum- (fixnum-quotient (least-fixnum) radix)))))

; Combine two numbers digitwise using op.

(define (combine-magnitudes m n op)
  (let recur ((m m) (n n) (carry (core->fixnum 0)))
    (if (and (zero-magnitude? m) (zero-magnitude? n))
	(fixnum->magnitude carry)
	(let ((result (fixnum+ carry (op (low-digit m) (low-digit n)))))
	  (call-with-values
            (lambda () (fixnum-quotient+remainder result radix))
	    (lambda (q r)
	      (if (fixnum-negative? r)
		  (adjoin-digit (fixnum+ r radix)
				(recur (high-digits m)
				       (high-digits n)
				       (fixnum- q (core->fixnum 1))))
		  (adjoin-digit r
				(recur (high-digits m)
				       (high-digits n)
				       q)))))))))

(define (add-magnitudes m n)
  (combine-magnitudes m n fixnum+))

(define (subtract-magnitudes m n)
  (combine-magnitudes m n fixnum-))

; Compare

(define (same-magnitude? m n)
  (let loop ((m m) (n n))
    (cond
     ((null? m)
      (null? n))
     ((null? n) #f)
     (else
      (and (fixnum=? (car m) (car n))
	   (loop (cdr m) (cdr n)))))))

(define (smaller-magnitude? m n)
  (let ((m-len (core->fixnum (length m)))
	(n-len (core->fixnum (length n))))
    (cond ((fixnum<? m-len n-len)
	   #t)
	  ((fixnum<? n-len m-len)
	   #f)
	  (else
	   (let loop ((m m) (n n) (a #f))
	     (cond ((zero-magnitude? m)
		    (or (not (zero-magnitude? n)) a))
		   ((zero-magnitude? n) #f)
		   (else
		    (loop (high-digits m)
			  (high-digits n)
			  (or (fixnum<? (low-digit m) (low-digit n))
			      (and (fixnum=? (low-digit m) (low-digit n))
                                   a))))))))))

; Multiply

(define (multiply-magnitudes m n)
  (let recur ((m m) (a zero-magnitude))
    (if (zero-magnitude? m)
	a
	(let ((a (combine-magnitudes
                  a n (lambda (d e) (fixnum+ d (fixnum* e (low-digit m)))))))
	  (adjoin-digit (low-digit a)
			(recur (high-digits m) (high-digits a)))))))

; Divide m/n: find q and r such that m = q*n + r, where 0 <= r < n.
; Oh no... time to get out Knuth...
; The main thing we don't do that Knuth does is to normalize the
; divisor (n) by shifting it left.

(define (divide-magnitudes m n cont)
  (if (zero-magnitude? (high-digits n))
      (divide-by-digit m
                       (low-digit n)
                       (lambda (q r)
			 (cont q (adjoin-digit r zero-magnitude))))
      (let recur ((m m) (cont cont))
	(if (smaller-magnitude? m n)
	    (cont zero-magnitude m)
	    (recur
	     (high-digits m)
	     (lambda (q r)
	       ;; 0 <= r < n  and  d < b
	       ;; so  b*r + d < b*n.
	       (divide-step (adjoin-digit (low-digit m) r)
			    n
			    (lambda (q1 r)
			      (cont (adjoin-digit q1 q) r)))))))))
	
; Divide m by n, where  n <= m < b*n, i.e. 1 <= quotient < b.
; E.g.  if  n = 100  then  100 <= m <= 999
;       if  n = 999  then  999 <= m <= 9989

(define (divide-step m n cont)
  (do ((m-high m (high-digits m-high))
       (n-high n (high-digits n-high)))
      ((zero-magnitude? (high-digits (high-digits n-high)))
       ;; Occasionally q^ is one larger than the actual first digit.
       ;; This loop will never iterate more than once.
       (let loop ((q^ (fixnum-min (guess-quotient-digit m-high n-high)
                                  (fixnum- radix (core->fixnum 1)))))
	 (let ((r (combine-magnitudes m n (lambda (d e)
					    (fixnum- d (fixnum* e q^))))))
	   (if (improper-magnitude? r)
	       ;; (begin (write `(addback ,m ,n ,q^ ,r)) (newline) ...)
	       (loop (fixnum- q^ (core->fixnum 1)))
	       (cont q^ r)))))))

; Compute q such that [m1 m2 m3] = q*[n1 n2] + r with 0 <= r < [n1 n2]
; Can assume b <= [0 n1 n2] <= [m1 m2 m3] <= [n1 n2 b-1]
; Some examples:
;  m / n :  100[1] / 10[02], 099 / 10, 099[1] / 99[0], 999[8] / 99[99]
; Various hacks are possible to improve performance.  In particular, the
; second division can be eliminated if the divisor is normalized.
; See Knuth.
;  [m1 m2] = q0*[n1] + r0
;  [m1 m2 m3] = q0*[n1 n2] + r^
;  r^ = b*r0 + m3 - q0*n2

(define (guess-quotient-digit m n)
  (let ((n1 (low-digit (high-digits n)))
	(n2 (low-digit n))
	(m1 (low-digit (high-digits (high-digits m))))
	(m2 (low-digit (high-digits m)))
	(m3 (low-digit m)))
    (let ((m12 (fixnum+ (fixnum* m1 radix) m2)))
      (call-with-values
        (lambda () (fixnum-quotient+remainder m12 n1))
	(lambda (q0 r0)
	  (let ((r^ (fixnum- (fixnum+ (fixnum* radix r0) m3) (fixnum* q0 n2)))
		(n12 (fixnum+ (fixnum* n1 radix) n2)))
	    (call-with-values
              (lambda () (fixnum-quotient+remainder r^ n12))
	      (lambda (q1 r1)
		(if (fixnum-positive? q1)
		    (begin (display "This should never happen: q1 = ")
			   (write q1) (newline)))
		(let ((q (fixnum+ q0 q1)))
		  (if (fixnum-negative? r1)
                      (fixnum- q (core->fixnum 1))
                      q))))))))))

(define (improper-magnitude? m)
  (and (not (zero-magnitude? m))
       (or (fixnum-negative? (low-digit m))
	   (improper-magnitude? (high-digits m)))))

; Special case of division algorithm for single-digit divisor.

(define (divide-by-digit m d cont)
  (if (fixnum-zero? d)
      (error "integer division by zero" m d)
      (let recur ((m m) (cont cont))
	(if (and (zero-magnitude? (high-digits m))
		 (fixnum<? (low-digit m) d))
	    (cont zero-magnitude (low-digit m))
	    (recur (high-digits m)
		   (lambda (q r)
		     (let ((m1 (fixnum+ (low-digit m) (fixnum* radix r))))
		       (call-with-values
                         (lambda () (fixnum-quotient+remainder m1 d))
			 (lambda (q1 r1)
			   (cont (adjoin-digit q1 q) r1))))))))))

;(define (divide-test seed)
;  (let ((random (make-random seed)))
;    (let loop ()
;      (let* ((z1 (integer+ (random) (integer* (random) 10000000)))
;             (z2 (integer+ (random) (integer* (random) 10000000)))
;             (n (max z1 z2))
;             (r (min z1 z2))
;             (q (random))
;             (m (integer+ (integer* n q) r)))
;        (if (not (= n r))
;            (integer-divide m n
;                            (lambda (q1 r1)
;                              (if (and (= q q1) (= r r1))
;                                  (begin (display ".")
;                                         (force-output (current-output-port)))
;                                  (error "division error" m n q q1 r r1)))))
;        (loop)))))


; from Larceny:

; Takes a bignum and a radix and returns the string which is the printable
; representation of the bignum in that radix.
;
; Uses brute force with extreme prejudice.

(define (bignum->string b r)
  (if (bignum-zero? b)
      (string-copy "0")
      (let ((r (fixnum->bignum r))
	    (d "0123456789abcdef")
	    (s (bignum-negative? b)))
	(let loop ((b (bignum-abs b)) (l '()))
	  (if (bignum-zero? b)
	      (if s
		  (list->string (cons #\- l))
		  (list->string l))
	      (bignum-divide->bignums
	       b r
	       (lambda (q r)
		 (loop q
		       (cons (string-ref d (bignum->core r))
			     l))))))))) 

(define (bignum-divide->bignums m n cont)
  (divide-magnitudes
   (bignum-magnitude m)
   (bignum-magnitude n)
   (lambda (q r)
     (cont (make-bignum (fixnum* (bignum-sign m) (bignum-sign n)) q)
	   (make-bignum (bignum-sign m) r)))))
)
