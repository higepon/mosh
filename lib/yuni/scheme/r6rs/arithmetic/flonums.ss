(library (yuni scheme r6rs arithmetic flonums)

  (export
   flonum?
   real->flonum
   fl=? fl<? fl>? fl<=? fl>=?
   flinteger? flzero? flpositive? flnegative? flodd? fleven?
   flfinite? flinfinite? flnan?
   flmax flmin
   fl+ fl* fl- fl/
   flabs
   fldiv-and-mod fldiv flmod
   fldiv0-and-mod0 fldiv0 flmod0
   flnumerator fldenominator
   flfloor flceiling fltruncate flround
   flexp fllog flsin flcos fltan flasin flacos flatan
   flsqrt flexpt

;  FIXME
;  &no-infinities make-no-infinities-violation no-infinities-violation
;  &no-nans make-no-nans-violation no-nans-violation

   ;fixnum->flonum
   )
  (import
    (yuni scheme refimpl arithmetic r6rs flonums)
    (only (rnrs) define assertion-violation quote list)
    )
(define (ni sym x)
  (assertion-violation sym "not implemented yet!"
                       x))
  
(define (real->flonum x)
  (ni 'real->flonum x))
(define (fldiv0-and-mod0 x y)
  (ni 'fldiv0-and-mod0 (list x y)))
(define (fldiv0 x y)
  (ni 'fldiv0 (list x y)))
(define (flmod0 x y)
  (ni 'flmod0 (list x y)))
(define (flnumerator x)
  (ni 'flnumerator x))
(define (fldenominator x)
  (ni 'fldenominator x))
  )
