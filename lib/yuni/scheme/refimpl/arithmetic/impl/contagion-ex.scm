(library (yuni scheme refimpl arithmetic impl contagion-ex)
         (export contagion/ex
                 pcontagion/ex
                 econtagion/ex
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl arithmetic-util)
                 (yuni scheme refimpl arithmetic impl coercion)
                 (yuni scheme refimpl arithmetic impl bignum)
                 (yuni scheme refimpl arithmetic impl contagion)
                 )
; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Contagion for exact arithmetic

(define (oops a b retry)
  (error "INTERNAL ERROR in contagion: same-representation arithmetic"
         a b retry)
  #t)

(define (no a b retry)
  (error "inexact operand to exact arithmetic"
         a b retry)
  #t)

; Standard matrix: for arithmetic operations.

(define cmatrix (make-contagion-matrix))

(define contagion/ex (lambda (a b retry)
		       (do-contagion cmatrix a b retry)))

(define econtagion/ex (lambda (a b retry)
			(do-contagion ematrix a b retry)))

(define pcontagion/ex (lambda (a b retry)
			(do-contagion pmatrix a b retry)))

; Predicate matrix: for <, <=, >, >=

(define pmatrix (make-contagion-matrix))

; Equality matrix: for = (only)

; #### I think this is identical with pmatrix
(define ematrix (make-contagion-matrix))

(define-contagion cmatrix fix fix fixnum->bignum fixnum->bignum)
(define-contagion cmatrix fix big fixnum->bignum id)
(define-contagion cmatrix fix rat fixnum->ratnum id)
(define-contagion cmatrix fix rec fixnum->recnum id)
(define-contagion cmatrix fix flo no symmetric)
(define-contagion cmatrix fix comp no symmetric)

(define-contagion cmatrix big big oops)
(define-contagion cmatrix big rat bignum->ratnum id)
(define-contagion cmatrix big rec bignum->recnum id)
(define-contagion cmatrix big flo no symmetric)
(define-contagion cmatrix big comp no symmetric)

(define-contagion cmatrix rat rat oops)
(define-contagion cmatrix rat rec ratnum->recnum id)
(define-contagion cmatrix rat flo no symmetric)
(define-contagion cmatrix rat comp no symmetric)

(define-contagion cmatrix rec rec oops)
(define-contagion cmatrix rec flo no symmetric)
(define-contagion cmatrix rec comp no symmetric)

(define-contagion cmatrix flo flo no)
(define-contagion cmatrix flo comp no symmetric)

(define-contagion cmatrix comp comp no)


(define-contagion pmatrix fix fix oops)
(define-contagion pmatrix fix big fixnum->bignum id)
(define-contagion pmatrix fix rat fixnum->ratnum id)
(define-contagion pmatrix fix rec fixnum->recnum id)
(define-contagion pmatrix fix flo no symmetric)
(define-contagion pmatrix fix comp no symmetric)

(define-contagion pmatrix big big oops)
(define-contagion pmatrix big rat bignum->ratnum id)
(define-contagion pmatrix big rec bignum->recnum id)
(define-contagion pmatrix big flo no symmetric)
(define-contagion pmatrix big comp no symmetric)

(define-contagion pmatrix rat rat oops)
(define-contagion pmatrix rat rec ratnum->recnum id)
(define-contagion pmatrix rat flo no symmetric)
(define-contagion pmatrix rat comp no symmetric)

(define-contagion pmatrix rec rec oops)
(define-contagion pmatrix rec flo no symmetric)
(define-contagion pmatrix rec comp no symmetric)

(define-contagion pmatrix flo flo no)
(define-contagion pmatrix flo comp no symmetric)

(define-contagion pmatrix comp comp no)


(define-contagion ematrix fix fix oops)
(define-contagion ematrix fix big fixnum->bignum id)
(define-contagion ematrix fix rat fixnum->ratnum id)
(define-contagion ematrix fix rec fixnum->recnum id)
(define-contagion ematrix fix flo no symmetric)
(define-contagion ematrix fix comp no symmetric)

(define-contagion ematrix big big oops)
(define-contagion ematrix big rat bignum->ratnum id)
(define-contagion ematrix big rec bignum->recnum id)
(define-contagion ematrix big flo no symmetric)
(define-contagion ematrix big comp no symmetric)

(define-contagion ematrix rat rat oops)
(define-contagion ematrix rat rec ratnum->recnum id)
(define-contagion ematrix rat flo no symmetric)
(define-contagion ematrix rat comp no symmetric)

(define-contagion ematrix rec rec oops)
(define-contagion ematrix rec flo no symmetric)
(define-contagion ematrix rec comp no symmetric)

(define-contagion ematrix flo flo no symmetric)
(define-contagion ematrix flo comp no symmetric)

(define-contagion ematrix comp comp no)


)
