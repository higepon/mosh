#!r6rs
(library (r7b-impl division)
         (export
;; from R7RS draft 4
ceiling-quotient ceiling-remainder ceiling/ centered-quotient centered-remainder
centered/ euclidean-quotient euclidean-remainder euclidean/ floor-quotient
floor-remainder floor/ round-quotient round-remainder round/ truncate-quotient
truncate-remainder truncate/
          )
         (import (rnrs) (rnrs r5rs))

;; ???: Inexact integers are allowed??

(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define (truncate/ x y) 
  (if (or (inexact? x) (inexact? y))
    (values (inexact (truncate-quotient x y))
            (inexact (truncate-remainder x y)))
    (values (truncate-quotient x y)
            (truncate-remainder x y))))

;; FIXME: accepts inexacts
(define euclidean-quotient div)
(define euclidean-remainder mod)
(define euclidean/ div-and-mod)

;; FIXME: accepts inexacts
(define centered-quotient div0)
(define centered-remainder mod0)
(define centered/ div0-and-mod0)

(define-syntax %define-division
  (syntax-rules ()
    ((_ fix quo rem q+r)
     (begin
       (define (quo x y)
         (exact (fix (/ x y))))
       (define (rem x y)
         (- x (* (quo x y) y)))
       (define (q+r x y)
         (let ((q (quo x y)))
           (values q
                   (- x (* q y)))))))))

(%define-division
  ceiling
  ceiling-quotient
  ceiling-remainder
  ceiling/)

(%define-division
  round
  round-quotient
  round-remainder
  round/)

(%define-division
  floor
  floor-quotient
  floor-remainder0 ;; Most implementation has native modulo
  floor/)

(define floor-remainder modulo)

)


