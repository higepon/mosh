(library (yuni scheme refimpl arithmetic impl flonum2string)
         (export flonum->string)
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl flonum-ieee)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl flonum2rational)
                 )

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting flonums to strings

; precision may be #f or an exact, positive integer

(define (flonum->string x radix precision)
  (cond
   ((flnan? x)
    (string-copy "+nan.0"))
   ((flinfinite? x)
    (if (flpositive? x)
	(string-copy "+inf.0")
	(string-copy "-inf.0")))
   ((flzero? x)
    (if (integer=? radix (core->integer 10))
	(string-copy (if (integer>=? (flsign x) (core->integer 0)) "0.0" "-0.0"))
	(string-copy "#i0")))		; #### sign?
   (else
    (let* ((exp (flexponent x))
	   (digit-count (flonum-exponent->digit-count exp))
	   (significand (flsignificand x))
	   (rep (string-append 
		 (if (flnegative? x) "-" "")
		 (if (integer=? radix (core->integer 10))
		     (burger10 (flabs x) significand exp)
		     (burger (flabs x) significand exp radix)))))
      (if (integer=? digit-count (core->integer 53))
	  (if precision
	      (string-append rep "|"
			     (integer->string
			      (integer-max precision (mantissa-width significand))
			      radix))
	      rep)
	  (string-append rep "|"
			 (integer->string (integer-max digit-count
						       (or precision (core->integer 0)))
					  radix)))))))

(define (mantissa-width significand)
  (let ((right-aligned
	 (let loop ((significand significand))
	   (call-with-values
	       (lambda () (integer-quotient+remainder significand (core->integer 2)))
	     (lambda (q r)
	       (if (integer-zero? r)
		   (loop q)
		   significand))))))
    (let loop ((significand right-aligned)
	       (width (core->integer 0)))
      (if (integer-zero? significand)
	  width
	  (loop (integer-quotient significand (core->integer 2))
		(integer+ (core->integer 1) width))))))

(define (burger x mantissa exp radix)
  (call-with-values
      (lambda ()
	(flonum->digits x mantissa exp
			fl-ieee-min-exponent/denormalized fl-ieee-mantissa-width
			(core->integer 2) radix))
    (lambda (k digits)
      (format digits k))))

(define (burger10 x mantissa exp)
  (call-with-values
      (lambda ()
	(flonum->digits10 x mantissa exp))
    (lambda (k digits)
      (format digits k))))

; from Bob Burger

;;; Free-format algorithm for printing IEEE double-precision positive
;;; floating-point numbers in base 10

;;; It uses the floating-point logarithm to estimate the scaling factor
;;; and a table to look up powers of ten.

;;; Input to flonum->digits:
;;;       v -- a positive floating-point number, f x 2^e
;;;       f -- mantissa of v
;;;       e -- exponent of v

;;; Output: k (d_1 d_2 ... d_n),
;;;   where 0.d_1...d_n x 10^k is the shortest correctly rounded base-10
;;;   number that rounds to v when input (it assumes the input
;;;   routine rounds to even)

;;; See also "Printing Floating-Point Numbers Quickly and Accurately"
;;; in Proceedings of the SIGPLAN '96 Conference on Programming Language
;;; Design and Implementation.

;;; Author: Bob Burger  Date: March 1996

(define flonum->digits10
  (lambda (v f e)
    (let ((bp-1 (integer-expt (core->integer 2)
			      (integer- fl-ieee-mantissa-width (core->integer 1))))
	  (round? (integer-even? f)))
      (if (integer>=? e (core->integer 0))
	  (if (not (integer=? f bp-1))
	      (let ((be (integer-expt (core->integer 2) e)))
		(scale10 (integer* f be 2) (core->integer 2) be be (core->integer 0) round? round? v))
	      (let ((be (integer-expt (core->integer 2) e)))
		(scale10 (integer* f be (core->integer 4)) 
			 (core->integer 4)
			 (integer* be (core->integer 2))
			 be
			 (core->integer 0)
			 round? round? v)))
	  (if (or (integer=? e fl-ieee-min-exponent/denormalized) (not (integer=? f bp-1)))
	      (scale10 (integer* f (core->integer 2)) 
		       (integer-expt (core->integer 2) (integer- (core->integer 1) e))
		       (core->integer 1) (core->integer 1) (core->integer 0)
		       round? round? v)
	      (scale10 (integer* f (core->integer 4))
		       (integer-expt (core->integer 2) (integer- (core->integer 2) e))
		       (core->integer 2) (core->integer 1) (core->integer 0)
		       round? round? v))))))

(define scale10
  (lambda (r s m+ m- k low-ok? high-ok? v)
    (let ((est (flonum->integer (flceiling (fl- (fllog10 v) (core->flonum 1e-10))))))
      (if (integer>=? est (core->integer 0))
          (fixup10 r (integer* s (expt10 est)) m+ m- est low-ok? high-ok?)
          (let ((scale (expt10 (integer-negate est))))
            (fixup10 (integer* r scale) s (integer* m+ scale) (integer* m- scale)
		     est low-ok? high-ok?))))))

(define fixup10
  (lambda (r s m+ m- k low-ok? high-ok?)
    (if ((if high-ok? integer>=? integer>?) (integer+ r m+) s) ; too low?
        (values (integer+ k (core->integer 1))
		(generate10 r s m+ m- low-ok? high-ok?))
        (values k
		(generate10 (integer* r (core->integer 10))
			    s
			    (integer* m+ (core->integer 10))
			    (integer* m- (core->integer 10))
			    low-ok? high-ok?)))))

(define generate10
  (lambda (r s m+ m- low-ok? high-ok?)
    (call-with-values
	(lambda () (integer-quotient+remainder r s))
      (lambda (d r)
	(let ((tc1 ((if low-ok? integer<=? integer<?) r m-))
	      (tc2 ((if high-ok? integer>=? integer>?) (integer+ r m+) s)))
	  (if (not tc1)
	      (if (not tc2)
		  (cons d (generate10 (integer* r (core->integer 10)) s 
				      (integer* m+ (core->integer 10))
				      (integer* m- (core->integer 10))
				      low-ok? high-ok?))
		  (list (integer+ d (core->integer 1))))
	      (if (not tc2)
		  (list d)
		  (if (integer<? (integer* r (core->integer 2)) s)
		      (list d)
		      (list (integer+ d (core->integer 1)))))))))))

(define expt10
  (let ((table (make-vector 326)))
    (do ((k (core->integer 0)
	    (integer+ k (core->integer 1)))
	 (v (core->integer 1)
	    (integer* v (core->integer 10))))
        ((integer=? k (core->integer 326)))
      (vector-set! table (integer->core k) v))
    (lambda (k)
      (vector-ref table (integer->core k)))))

(define fllog10
  (let ((f (fl/ (core->flonum 1.0) (fllog (core->flonum 10.0)))))
    (lambda (x)
      (fl* (fllog x) f))))

;;; Fully general free-format algorithm for positive floating-point numbers

;;; It uses the floating-point logarithm to estimate the scaling factor
;;; and a table to look up powers of ten.

;;; Input to flonum->digits:
;;;       v -- a positive floating-point number, f x b^e
;;;       f -- mantissa of v
;;;       e -- exponent of v
;;;   min-e -- smallest representable exponent
;;;       b -- input base (usually two)
;;;      ob -- output base (usually ten)

;;; Output: k (d_1 d_2 ... d_n),
;;;   where 0.d_1...d_n x ob^k is the shortest correctly rounded base-ob
;;;   number that rounds to v when input (it does not assume any particular
;;;   input rounding algorithm)

;;; See also "Printing Floating-Point Numbers Quickly and Accurately"
;;; in Proceedings of the SIGPLAN '96 Conference on Programming Language
;;; Design and Implementation.

;;; Author: Bob Burger  Date: March 1996

(define flonum->digits
  (lambda (v f e min-e p b ob)
    (if (integer>=? e (core->integer 0))
	(if (not (integer=? f (integer-expt b (integer- p (core->integer 1)))))
	    (let ((be (integer-expt b e)))
	      (scale (integer* f (integer* be (core->integer 2)))
		     (core->integer 2) be be (core->integer 0) ob #f #f v))
	    (let* ((be (integer-expt b e)) (be1 (integer* be b)))
	      (scale (integer* f (integer* be1 (core->integer 2)))
		     (integer* b (core->integer 2)) be1 be (core->integer 0) ob #f #f v)))
	(if (or (integer=? e min-e) (not (integer=? f (integer-expt b (integer- p (core->integer 1))))))
	    (scale (integer* f (core->integer 2)) (integer* (integer-expt b (integer-negate e)) (core->integer 2))
		   (core->integer 1) (core->integer 1) (core->integer 0) ob #f #f v)
	    (scale (integer* f (integer* b (core->integer 2))) 
		   (integer* (integer-expt b (integer- (core->integer 1) e)) (core->integer 2))
		   b (core->integer 1) (core->integer 0) ob #f #f v)))))

(define scale
  (lambda (r s m+ m- k ob low-ok? high-ok? v)
    (let ((est (flonum->integer (flceiling (fl- (logB ob v) (core->flonum 1e-10))))))
      (if (integer>=? est (core->integer 0))
          (fixup r (integer* s (exptt ob est)) m+ m- est ob low-ok? high-ok?)
          (let ((scale (exptt ob (integer-negate est))))
            (fixup (integer* r scale) s (integer* m+ scale) (integer* m- scale)
		   est ob low-ok? high-ok?))))))

(define fixup
  (lambda (r s m+ m- k ob low-ok? high-ok?)
    (if ((if high-ok? integer>=? integer>?) (integer+ r m+) s) ; too low?
        (values (integer+ k (core->integer 1)) (generate r s m+ m- ob low-ok? high-ok?))
        (values k
		(generate (integer* r ob) s (integer* m+ ob) (integer* m- ob) ob low-ok? high-ok?)))))

(define generate
  (lambda (r s m+ m- ob low-ok? high-ok?)
    (call-with-values
	(lambda () (integer-quotient+remainder r s))
      (lambda (d r)
	(let ((tc1 ((if low-ok? integer<=? integer<?) r m-))
	      (tc2 ((if high-ok? integer>=? integer>?) (integer+ r m+) s)))
	  (if (not tc1)
	      (if (not tc2)
		  (cons d (generate (integer* r ob) s (integer* m+ ob) (integer* m- ob)
				    ob low-ok? high-ok?))
		  (list (integer+ d (core->integer 1))))
	      (if (not tc2)
		  (list d)
		  (if (integer<? (integer* r (core->integer 2)) s)
		      (list d)
		      (list (integer+ d (core->integer 1)))))))))))

(define exptt
  (let ((table (make-vector 326)))
    (do ((k (core->integer 0)
	    (integer+ k (core->integer 1)))
	 (v (core->integer 1)
	    (integer* v (core->integer 10))))
        ((integer=? k (core->integer 326)))
      (vector-set! table (integer->core k) v))
    (lambda (B k)
      (if (and (integer=? B (core->integer 10))
	       (integer<=? (core->integer 0) k)
	       (integer<=? k (core->integer 325)))
          (vector-ref table (integer->core k))
          (integer-expt B k)))))

(define logB
  (let ((table (make-vector 37)))
    (do ((B (core->integer 2) (integer+ B (core->integer 1))))
        ((integer=? B (core->integer 37)))
      (vector-set! table (integer->core B)
		   (fl/ (core->flonum 1.0) (fllog (fixnum->flonum B))))) ; assumes a certain fixnum range
    (lambda (B x)
      (if (and (integer<=? (core->integer 2) B)
	       (integer<=? B (core->integer 36)))
          (fl* (fllog x) (vector-ref table (integer->core B)))
          (fl/ (fllog x) (fllog B))))))

; end from Bob Burger

; from Larceny

; MacScheme's traditional heuristic-only format.
;
; Given a nonempty list of digits (not characters) in reverse order,
; and an exponent n such that the value is 0.f*radix^n, where f is the
; integer represented by the digits, returns a string.

(define (format digits e)
  (let* ((s (list->string
	     (map (lambda (digit)
		    (vector-ref **digit-characters** (integer->core digit)))
		  digits)))
         (n (integer- e (core->integer 1))))
    (cond ((integer<? n (core->integer -5)) (exponential-format s n))
          ((integer>? n (core->integer 8)) (exponential-format s n))
          (else (decimal-format s (integer- e (core->integer (string-length s))))))))

(define **digit-characters**
  '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define (exponential-format s n)
  (string-append (substring s 0 1)
                 "."
                 (if (integer>? (core->integer (string-length s)) (core->integer 1))
                     (substring s 1 (string-length s))
                     "0")
                 "e"
                 (integer->string n (core->integer 10))))

(define (decimal-format s n)
  (let ((k (core->integer (string-length s))))
    (cond ((integer-negative? n)
           (if (integer-positive? (integer+ n k))
               (string-append (substring s 0 (integer->core (integer+ n k)))
                              "."
                              (substring s (integer->core (integer+ n k)) (integer->core k)))
               (string-append "0."
                              (make-string (integer->core (integer- (core->integer 0) (integer+ n k))) #\0)
                              s)))
          (else (string-append s (make-string (integer->core n) #\0) ".0")))))

(define (flonum-exponent->digit-count exp)
  (if (integer>=? exp fl-ieee-min-exponent)
      fl-ieee-mantissa-width
      (integer- (integer+ exp fl-ieee-mantissa-width)
		fl-ieee-min-exponent/denormalized)))
)
