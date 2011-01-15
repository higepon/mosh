(library (yuni scheme refimpl arithmetic r6rs base)
         (export
           ;; basic op
           * + - / < <= = > >=

           ;;
           abs acos asin atan cos
           ceiling
           complex?
           denominator
           exp expt floor
           gcd imag-part exact? inexact? integer?
           lcm log magnitude
           make-polar make-rectangular
           max min
           negative?
           number? numerator
           odd?
           number->string
           string->number
           positive?
           rational?
           rationalize
           real-part
           real?
           round
           sin
           sqrt
           tan
           truncate
           zero?
           real-valued?
           rational-valued?
           integer-valued?
           exact
           inexact
           finite?
           infinite?
           nan?
           div
           mod
           div-and-mod
           div0
           mod0
           div0-and-mod0
           exact-integer-sqrt
           angle )
         (import 
           (yuni scheme refimpl arithmetic impl number2string)
           (yuni scheme refimpl arithmetic impl string2number)
           (yuni scheme refimpl arithmetic impl generic-ex)
           (yuni scheme refimpl arithmetic impl generic)))



