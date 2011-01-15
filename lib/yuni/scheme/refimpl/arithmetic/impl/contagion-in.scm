; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Contagion for exact arithmetic

(define (oops a b retry)
  (error "INTERNAL ERROR in contagion: same-representation arithmetic"
         a b retry)
  #t)

(define (no a b retry)
  (error "exact operand to inexact arithmetic"
         a b retry)
  #t)

; One standard matrix is enough

(define cmatrix (make-contagion-matrix))

(define-contagion cmatrix fix fix no symmetric)
(define-contagion cmatrix fix big no symmetric)
(define-contagion cmatrix fix rat no symmetric)
(define-contagion cmatrix fix rec no symmetric)
(define-contagion cmatrix fix flo no symmetric)
(define-contagion cmatrix fix comp no symmetric)

(define-contagion cmatrix big big no)
(define-contagion cmatrix big rat no symmetric)
(define-contagion cmatrix big rec no symmetric)
(define-contagion cmatrix big flo no symmetric)
(define-contagion cmatrix big comp no symmetric)

(define-contagion cmatrix rat rat no)
(define-contagion cmatrix rat rec no symmetric)
(define-contagion cmatrix rat flo no symmetric)
(define-contagion cmatrix rat comp no symmetric)

(define-contagion cmatrix rec rec no)
(define-contagion cmatrix rec flo no symmetric)
(define-contagion cmatrix rec comp no symmetric)

(define-contagion cmatrix flo flo oops)
(define-contagion cmatrix flo comp flonum->compnum id)

(define-contagion cmatrix comp comp oops)

(define contagion/in (lambda (a b retry)
		       (do-contagion cmatrix a b retry)))

(define econtagion/in (lambda (a b retry)
			(do-contagion cmatrix a b retry)))

(define pcontagion/in (lambda (a b retry)
			(do-contagion cmatrix a b retry)))

