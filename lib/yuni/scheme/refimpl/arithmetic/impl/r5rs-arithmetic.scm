; Synonyms for all R5RS arithmetic operations that might be used
; by the reference implementation.

(define core:eqv? eqv?)
(define core:number? number?)
(define core:complex? complex?)
(define core:real? real?)
(define core:rational? rational?)
(define core:integer? integer?)

(define core:exact? exact?)
(define core:inexact? inexact?)

(define core:= =)
(define core:< <)
(define core:> >)
(define core:<= <=)
(define core:>= >=)

(define core:zero? zero?)
(define core:positive? positive?)
(define core:negative? negative?)
(define core:odd? odd?)
(define core:even? even?)

(define core:max max)
(define core:min min)

(define core:+ +)
(define core:* *)
(define core:- -)
(define core:/ /)

(define core:abs abs)

(define core:quotient quotient)
(define core:remainder remainder)
(define core:modulo modulo)

(define core:gcd gcd)
(define core:lcm lcm)

(define core:numerator numerator)
(define core:denominator denominator)

(define core:floor floor)
(define core:ceiling ceiling)
(define core:truncate truncate)
(define core:round round)

(define core:rationalize rationalize)

(define core:exp exp)
(define core:log log)
(define core:sin sin)
(define core:cos cos)
(define core:tan tan)
(define core:asin asin)
(define core:acos acos)
(define core:atan atan)

(define core:sqrt sqrt)
(define core:expt expt)

(define core:make-rectangular make-rectangular)
(define core:make-polar make-polar)
(define core:real-part real-part)
(define core:imag-part imag-part)
(define core:magnitude magnitude)
(define core:angle angle)

(define core:exact->inexact exact->inexact)
(define core:inexact->exact inexact->exact)

(define core:number->string number->string)
(define core:string->number string->number)

