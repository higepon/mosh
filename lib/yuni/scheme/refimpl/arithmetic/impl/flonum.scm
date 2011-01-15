(library (yuni scheme refimpl arithmetic impl flonum)
         (export flonum->core
                 fl*
                 fllog
                 fl/
                 core->flonum
                 fixnum->flonum
                 fl=?
                 flinf+
                 flinf-
                 flnan?
                 flinteger?
                 flceiling
                 fl-
                 flinfinite?
                 flpositive?
                 flzero?
                 flnegative?
                 flabs
                 flexp
                 flcos
                 flsin
                 flsqrt
                 fl+
                 flatan
                 flonum?
                 flnan
                 flacos
                 flasin
                 fltan
                 flfloor
                 flquotient+remainder
                 flremainder
                 flquotient
                 flmax
                 flmin
                 flfinite?
                 fleven?
                 flodd?
                 fl>?
                 fl>=?
                 fl<?
                 flonum->fixnum
                 fl<=?
                 (rename (fldiv+mod fldiv-and-mod))
                 fldiv
                 flmod
                 fltruncate
                 flround
                 flexpt
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl custom)
                 (yuni scheme refimpl arithmetic impl bitwise)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 )

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Flonums in terms of R5RS; assumes underlying IEEE-like representation

(define* :flonum
  (inexact))

(define (really-make-flonum x)
  (make :flonum
        (inexact x)))

(define (flonum? x)
  (is-a? x :flonum))

(define (flonum-inexact x)
  (let-with x (inexact) inexact))


#|
; SRFI 9
(define-record-type :flonum
  (really-make-flonum inexact)
  flonum?
  (inexact flonum-inexact))

; Scheme 48 extension; comment out if not available
(define-record-discloser :flonum
  (lambda (r)
    (list 'flonum (flonum-inexact r))))
|#

(define (make-flonum n)
  (really-make-flonum (core:exact->inexact n)))

(define core->flonum make-flonum)

(define flonum->core flonum-inexact)

; for playing around
(define fl make-flonum)

(define (make-fl*fl->fl core-op)
  (lambda (a b)
    (if (or (flnan? a)
	    (flnan? b))
	flnan
	(make-flonum (core-op (flonum-inexact a) (flonum-inexact b))))))

(define fl+/2 (make-fl*fl->fl core:+))
(define (fl+ . args)
  (reduce (make-flonum 0.0) fl+/2 args))

(define fl-/2 (make-fl*fl->fl core:-))
(define (fl- arg0 . args)
  (reduce (make-flonum 0.0) fl-/2 (cons arg0 args)))

(define (make-fl->fl core-op)
  (lambda (a)
    (if (flnan? a)
	flnan
	(make-flonum (core-op (flonum-inexact a))))))

(define fl*/2 (make-fl*fl->fl core:*))
(define (fl* . args)
  (reduce (make-flonum 1.0) fl*/2 args))

(define (/* a b)
  (cond
   ((core:= b core-inf+)
    (cond
     ((or (core:= a core-inf+) (core:= a core-inf-))
      core-nan)
     ((core:< a 0.0)
      -0.0)
     (else
      0.0)))
   ((core:= b core-inf-)
    (cond
     ((or (core:= a core-inf+) (core:= a core-inf-))
      core-nan)
     ((core:< a 0.0)
      0.0)
     (else
      -0.0)))
   ((not (core:= b 0.0)) (core:/ a b))
   ((core:= a 0.0) core-nan)
   ((core:> a 0.0) core-inf+)
   (else core-inf-)))

(define fl//2 (make-fl*fl->fl /*))
(define (fl/ arg0 . args)
  (reduce (make-flonum 1.0) fl//2 (cons arg0 args)))

(define (make-fl*fl->val core-op)
  (lambda (a b)
    (core-op (flonum-inexact a) (flonum-inexact b))))

(define fl=? (make-transitive-pred (make-fl*fl->val core:=)))
(define fl>=? (make-transitive-pred (make-fl*fl->val core:>=)))
(define fl<=? (make-transitive-pred (make-fl*fl->val core:<=)))
(define fl>? (make-transitive-pred (make-fl*fl->val core:>)))
(define fl<? (make-transitive-pred (make-fl*fl->val core:<)))

(define (make-fl->val core-op)
  (lambda (a)
    (core-op (flonum-inexact a))))

(define (flzero? x)
  (fl=? x (core->flonum 0.0)))
(define (flpositive? x)
  (fl>? x (core->flonum 0.0)))
(define (flnegative? x)
  (fl<? x (core->flonum 0.0)))

(define flmin (make-min/max fl<?))
(define flmax (make-min/max fl>?))

(define (flabs x)
  (if (flnegative? x)
      (fl- x)
      x))

(define flexp (make-fl->fl core:exp))

(define (log1* z)
  (cond
   ((core:= core-inf+ z)
    core-inf+)
   ((core:= core-inf- z)
    core-nan)
   ((not (core:= z z))
    core-nan)
   ((core:= 0.0 z)
    core-inf-)
   (else
    (core:log z))))

(define fllog1 (make-fl->fl log1*))
(define flsin (make-fl->fl core:sin))
(define flcos (make-fl->fl core:cos))
(define fltan (make-fl->fl core:tan))
(define flasin (make-fl->fl core:asin))
(define flacos (make-fl->fl core:acos))
(define flatan1 (make-fl->fl core:atan))
(define flatan2 (make-fl*fl->fl core:atan))

(define (fllog z . extra)
  (if (null? extra)
      (fllog1 z)
      (fl/ (fllog1 z)
	   (fllog1 (car extra)))))

(define (flatan x . extra)
  (if (null? extra)
      (flatan1 x)
      (flatan2 x (car extra))))

(define (sqrt* z)
  (cond
   ((core:= core-inf+ z)
    core-inf+)
   ((core:= core-inf- z)
    core-nan)
   ((core:< z 0.0)
    core-nan)
   ((not (core:= z z))
    core-nan)
   (else
    (core:sqrt z))))

(define flsqrt (make-fl->fl sqrt*))

(define (expt* a b)
  (cond
   ((core:> a 0.0)
    (cond ((core:> b 0.0)
           (core:expt a b))
          ((core:= b 0.0)
           a)
          (else
           (core:/ 1.0 (expt* a (core:- b))))))
   ((core:= a 0.0)
    (cond ((core:> b 0.0)
           0.0)
          ((core:= b 0.0)
           1.0)
          (else
           core-nan)))
   (else
    (cond ((core:= b 0.0)
           1.0)
          (else
           core-nan)))))

(define flexpt (make-fl*fl->fl core:expt))

(define flfloor (make-fl->fl core:floor))
(define flceiling (make-fl->fl core:ceiling))
(define fltruncate (make-fl->fl core:truncate))
(define flround (make-fl->fl core:round))

(define (fixnum->flonum fx)
  (make-flonum (fixnum->core fx)))

(define (flonum->fixnum f)
  (cond
   ((fl<? f (fixnum->flonum (least-fixnum)))
    (least-fixnum))
   ((fl>? f (fixnum->flonum (greatest-fixnum)))
    (greatest-fixnum))
   (else
    (core->fixnum (core:inexact->exact (core:round (flonum-inexact f)))))))

; FIXME: Are these still used?

(define flquotient (make-fl*fl->fl core:quotient))
(define flremainder (make-fl*fl->fl core:remainder))
(define (flquotient+remainder a b)
  (values (flquotient a b)
	  (flremainder a b)))
(define flmodulo (make-fl*fl->fl core:modulo))

(define (fldiv+mod x y)
  (if (flzero? y)
      (values flnan flnan)
      (let* ((div (flfloor (fl/ x y)))
             (mod (fl- x (fl* div y))))
        (values div mod))))

(define (fldiv x y)
  (call-with-values
   (lambda () (fldiv+mod x y))
   (lambda (d m)
     d)))

(define (flmod x y)
  (call-with-values
   (lambda () (fldiv+mod x y))
   (lambda (d m)
     m)))

(define flodd?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x core-inf+)
             (core:= x core-inf-)
             (not (core:= x x)))
         #f
         (core:odd? x)))))

(define fleven?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x core-inf+)
             (core:= x core-inf-)
             (not (core:= x x)))
         #f
         (core:even? x)))))

(define flinteger?
  (make-fl->val
   (lambda (x)
     (if (or (core:= x core-inf+)
             (core:= x core-inf-)
             (not (core:= x x)))
         #f
         (core:integer? x)))))

(define core-inf+ 1e1025)
(define core-inf- -1e1025)
(define core-nan (core:- core-inf+ core-inf+))

(define flinf+ (make-flonum core-inf+))
(define flinf- (make-flonum core-inf-))

(define flnan (make-flonum core-nan))

(define flnan? (make-fl->val (lambda (x) (not (core:= x x)))))

(define (infinite?* x)
  (or (core:= x core-inf+)
      (core:= x core-inf-)))

(define flinfinite? (make-fl->val infinite?*))

(define (finite?* x)
  (and (core:= x x)
       (not (infinite?* x))))

(define flfinite? (make-fl->val finite?*))

)
