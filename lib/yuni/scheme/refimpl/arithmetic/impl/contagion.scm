(library (yuni scheme refimpl arithmetic impl contagion)
         (export define-contagion
                 do-contagion
                 make-contagion-matrix
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl fixnum)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 (yuni scheme refimpl arithmetic impl recnum)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl compnum)
                 )
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Utilities for implementing contagion

; Indices for the various numerical types

(define fixtype  0)
(define bigtype  1)
(define rattype  2)
(define rectype  3)
(define flotype  4)
(define comptype 5)

(define-syntax numtype-enum
  (syntax-rules (fix big rat rec flo comp)
    ((numtype-enum fix) fixtype)
    ((numtype-enum big) bigtype)
    ((numtype-enum rat) rattype)
    ((numtype-enum rec) rectype)
    ((numtype-enum flo) flotype)
    ((numtype-enum comp) comptype)))

(define (do-contagion matrix a b retry)

  ;; number-type is highly bummed but can be made even faster by
  ;; elminating the conds and instead simply shifting bytevector-like
  ;; tags (say) by three bits, thereby creating a unique index space
  ;; based on the type tag alone, and having contagion matrices with
  ;; a number of unused slots.  However, most time is probably not
  ;; spent in this procedure.

  (define (number-type x)
    (cond ((fixnum? x) fixtype)
	  ((bignum? x) bigtype)
	  ((ratnum? x) rattype)
	  ((recnum? x) rectype)
	  ((flonum? x) flotype)
	  ((compnum? x) comptype)
	  (else
	   (error "contagion error" a b retry))))

  (let* ((ta (number-type a))
	 (tb (number-type b))
	 (f  (vector-ref (vector-ref matrix ta) tb)))
    (f a b retry)))

(define (oops a b retry)
  (error "INTERNAL ERROR in contagion: undefined contagion"
         a b retry)
  #t)

; Contagion matrices. They are completely symmetric with respect to the
; final types, although the entries in the matrix are different across the
; diagonal.

(define (make-contagion-matrix)
  (let ((v (make-vector 6)))
    (vector-set! v 0 (make-vector 6 oops))
    (vector-set! v 1 (make-vector 6 oops)) 
    (vector-set! v 2 (make-vector 6 oops))
    (vector-set! v 3 (make-vector 6 oops))
    (vector-set! v 4 (make-vector 6 oops))
    (vector-set! v 5 (make-vector 6 oops))
    v))

(define (fun f1 f2)
  (lambda (a b retry)
    (retry (f1 a) (f2 b))))

(define-syntax define-contagion
  (syntax-rules (symmetric)

    ((define-contagion ?matrix ?type1 ?type2 ?f symmetric)
     (let* ((f ?f)
            (f-inv (lambda (a b retry)
                     (f b a retry))))
       (vector-set! (vector-ref ?matrix (numtype-enum ?type1))
                    (numtype-enum ?type2)
                    f)
       (vector-set! (vector-ref ?matrix (numtype-enum ?type2))
                    (numtype-enum ?type1)
                    f-inv)))


    ((define-contagion ?matrix ?type1 ?type2 ?coerce1 ?coerce2)
     (let ((coerce1 ?coerce1)
           (coerce2 ?coerce2))
       (vector-set! (vector-ref ?matrix (numtype-enum ?type1))
                    (numtype-enum ?type2)
                    (fun ?coerce1 ?coerce2))
       (vector-set! (vector-ref ?matrix (numtype-enum ?type2))
                    (numtype-enum ?type1)
                    (fun ?coerce2 ?coerce1))))

    ((define-contagion ?matrix ?type1 ?type2 ?f)
     (vector-set! (vector-ref ?matrix (numtype-enum ?type1))
                  (numtype-enum ?type2)
                  ?f))))
)
