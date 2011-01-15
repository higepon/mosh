; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Tests for converting between numbers and their string representations

; Probably assumes IEEE double precision arithmetics for the flonum
; examples.

(define (n-r5rs= a b)
  (= a (r5rs->number b)))

(check (string->number "100") ==> 100)
(check (string->number "100" (r5rs->number 16)) ==> 256)
(check (string->number "1e2") ==> 100.0)
(check (string->number "15##") ==> 1500.0)
(check (string->number "15000000000000000") ==> 15000000000000000)
(check (string->number "14/6") ==> 14/6)
(check (string->number "0.7") ==> 0.7)
(check (string->number "0.7|5") ==> 0.6875)
(check (string->number "+inf.0") ==> 1e1500)
(check (string->number "-inf.0") ==> -1e1500)
(check (= (string->number "+nan.0") (string->number "+nan.0")) => #f)
(check (string->number "#e1e10") ==> 10000000000)
(check (string->number "#e-1e10") ==> -10000000000)
(check (string->number "#i7/10") ==> 0.7)
(check (string->number "-0.0") ==> -0.0)


(define (check-string->number n r)
  (check (string->number (number->string n r) r) => (=) n))

(numerical check-string->number 100 10)
(numerical check-string->number 256 16)
(numerical check-string->number 1e2 10)
(numerical check-string->number 1500.0 10)
(numerical check-string->number 15000000000000000 10)
(numerical check-string->number 14/6 10)
(numerical check-string->number 0.7 10)
(numerical check-string->number 1e-323 10)
