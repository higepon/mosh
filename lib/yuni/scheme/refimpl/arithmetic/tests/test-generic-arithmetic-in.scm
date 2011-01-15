; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Tests for inexact arithmetic

(define (n-r5rs= a b)
  (inexact=? a (r5rs->number b)))

(check (numerical inexact-complex? 3.0+4i) =>  #t)
(check (numerical inexact-complex? 3) =>  #f)
(check (numerical inexact-real? 3) =>  #f)
(check (inexact-real? (numerical inexact-make-rectangular -2.5 0.0)) =>  #f)
(check (numerical inexact-real? -2.5) =>  #t)
(check (inexact-real? (string->number "#e1e10")) =>  #f)
(check (inexact-real? (string->number "-inf.0")) =>  #t)
(check (inexact-real? (string->number "+nan.0")) =>  #t)
(check (inexact-rational? (string->number "-inf.0")) =>  #f)
(check (inexact-rational? (string->number "+nan.0")) =>  #f)
(check (numerical inexact-rational? 6/10) =>  #f)
(check (numerical inexact-rational? #i6/3) =>  #t)
(check (inexact-integer? (numerical inexact-make-rectangular 3.0 0.0)) =>  #f)
(check (numerical inexact-integer? 3.0) =>  #t)
(check (numerical inexact-integer? #i8/4) =>  #t)

(check (inexact-number? (string->number "+nan.0")) =>  #t)
(check (inexact-complex? (string->number "+nan.0")) =>  #t)
(check (inexact-complex? (string->number "+inf.0")) =>  #t)
(check (inexact-real? (string->number "+nan.0")) =>  #t)
(check (inexact-real? (string->number "-inf.0")) =>  #t)
(check (inexact-rational? (string->number "+inf.0")) =>  #f)
(check (inexact-rational? (string->number "+nan.0")) =>  #f)
(check (inexact-integer? (string->number "-inf.0")) =>  #f)

(check (numerical inexact<=? 1.0 2.0 3.0 4.0) => #t)
(check (numerical inexact<? 1.0 2.0 #i7/2 #i4 9999999999999999999.0) => #t)
(check (numerical inexact>? 1.0 2.0 #i7/2 #i4 9999999999999999999.0) => #f)
(check (numerical inexact>=? 2.0 2.0 #i3/4 0.0) => #t)
(check (numerical inexact=? 0.0 -0.0) => #t)
(check (inexact=? (numerical inexact-make-rectangular 4.0 0.0) (r5rs->number 4.0)) => #t)
(check (inexact=? (numerical inexact-make-rectangular 4.0 0.0) 
		  (numerical inexact-make-rectangular 4.0 2.0)) => #f)
(check (inexact=? (numerical inexact-make-rectangular 4.0 2.0) (r5rs->number 4.0)) => #f)
(check (inexact=? (numerical inexact-make-rectangular 1e40 0.0) (r5rs->number 1e40)) => #t)

(check (numerical inexact-zero? 3218943724243.0) => #f)
(check (numerical inexact-zero? 0.0) => #t)
(check (numerical inexact-zero? -0.0) => #t)
(check (inexact-zero? (numerical inexact-make-rectangular 0.0 0.0)) => #t)

(check (numerical inexact-odd? 5.0) => #t)
(check (numerical inexact-odd? 5.0) => #t)
(check (numerical inexact-even? 5.0) => #f)
(check (numerical inexact-even? 5.0) => #f)
(check (numerical inexact-odd? -5.0) => #t)
(check (numerical inexact-odd? -5.0) => #t)
(check (numerical inexact-even? -5.0) => #f)
(check (numerical inexact-even? -5.0) => #f)

(check (numerical inexact-abs 7.0) ==> 7.0)
(check (numerical inexact-abs -7.0) ==> 7.0)
(check (numerical inexact-abs 0.7) ==> 0.7)
(check (numerical inexact-abs -0.7) ==> 0.7)

(check (numerical inexact-max 1.0 2.0 4.0 3.0 5.0) ==> 5.0)
(check (numerical inexact-max 1.0 2.0 3.0 5.0 4.0) ==> 5.0)
(check (numerical inexact-max 1.0 5.0 #i7/2 2.0 4.0) ==> 5.0)
(check (numerical inexact-min 4.0 1.0 2.0 3.0 5.0) ==> 1.0)
(check (numerical inexact-min 2.0 1.0 3.0 5.0 4.0) ==> 1.0)
(check (numerical inexact-min 1.0 5.0 #i7/2 2.0 4.0) ==> 1.0)

(check (numerical inexact+ 3.0 4.0) ==> 7.0)
(check (numerical inexact+ 3.0) ==> 3.0)
(check (numerical inexact+) ==> 0.0)
(check (numerical inexact+ 9999999999999.0 999999999999.0) ==> 10999999999998.0)
(check (numerical inexact+ 1000.0 5.0) ==> 1005.0)
(check (numerical inexact* 4.0) ==> 4.0)
(check (numerical inexact*) ==> 1.0)
(check (numerical inexact* 4.0 3000.0) ==> 12000.0)
(check (numerical inexact* 9999999999999.0 999999999999.0) ==> 9999999999989000000000001.0)

(check (numerical inexact- 3.0 4.0) ==> -1.0)
(check (numerical inexact- 3.0 4.0 5.0) ==> -6.0)
(check (numerical inexact- 3.0) ==> -3.0)
(check (numerical inexact/ 3.0 4.0 5.0) ==> #i3/20)
(check (numerical inexact/ 3.0) ==> #i1/3)
(check (numerical inexact/ 1.0 2.0) ==> 0.5)

(check (numerical inexact-gcd) ==> 0.0)
(check (numerical inexact-lcm 32.0 -36.0) ==> 288.0)
(check (numerical inexact-lcm) ==> 1.0)

(check (numerical inexact-floor -4.3) ==> -5.0)
(check (numerical inexact-ceiling -4.3) ==> -4.0)
(check (numerical inexact-truncate -4.3) ==> -4.0)
(check (numerical inexact-round -4.3) ==> -4.0)

(check (numerical inexact-floor 3.5) ==> 3.0)
(check (numerical inexact-ceiling 3.5) ==> 4.0)
(check (numerical inexact-truncate 3.5) ==> 3.0)
(check (numerical inexact-round 7.0) ==> 7.0)

(check (inexact-floor flinf+) => (inexact=?) flinf+)
(check (inexact-ceiling flinf-) => (inexact=?) flinf-)

(check (inexact-sqrt flinf+) => (inexact=?) flinf+)

(check (inexact-exp flinf+) => (inexact=?) flinf+)
(check (inexact-exp flinf-) ==> 0.0)
(check (inexact-log flinf+) => (inexact=?) flinf+)
(check (numerical inexact-log 0.0) => (inexact=?)  flinf-)
(check (inexact-atan flinf-) ==> -1.5707963267948965)
(check (inexact-atan flinf+) ==> 1.5707963267948965)

(check (numerical inexact-expt 5.0 3.0) ==>  125.0)
(check (numerical inexact-expt 5.0 -3.0) ==>  8.0e-3)
(check (numerical inexact-expt 5.0 0.0) ==> 1.0)
(check (numerical inexact-expt 0.0 5.0) ==>  0.0)
(check (numerical inexact-expt 0.0 5+.0000312i) ==>  0.0)
(check (numerical inexact-expt 0.0 0.0) ==>  1.0)

(check (inexact-numerator (numerical inexact/ 3.0 -4.0)) ==> -3.0)
(check (numerical inexact-denominator 0.0) ==> 1.0)

(check (inexact-angle flinf+) ==> 0.0)
(check (inexact-angle flinf-) ==> 3.141592653589793)

(check (numerical inexact-sqrt -4.0) ==> +2.0i)

