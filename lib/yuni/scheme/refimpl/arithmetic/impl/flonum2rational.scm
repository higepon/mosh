(library (yuni scheme refimpl arithmetic impl flonum2rational)
         (export flonum->integer
                 flonum->rational
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl integer)
                 (yuni scheme refimpl arithmetic impl flonum)
                 (yuni scheme refimpl arithmetic impl flonum-ieee)
                 (yuni scheme refimpl arithmetic impl rational)
                 (yuni scheme refimpl arithmetic impl ratnum)
                 )

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting from flonums to rationals.

(define (flonum->integer f)
  (if (flinteger? f)
      (flonum->rational f)
      (error "Can't convert to an integer" f)))

(define (flonum->rational f)
  (if (or (fl=? f flinf+)
	  (fl=? f flinf-)
	  (flnan? f))
      (error "Can't convert to an exact number" f))
  (let* ((m (flsignificand f))
	 (e (flexponent f))
	 (q  (if (integer>=? e (core->integer 0))
		 (integer* m (integer-expt (core->integer 2) e))
		 (integer/ m
			   (integer-expt (core->integer 2) (integer-negate e))))))
    (if (not (integer-zero? (flsign f)))
	(rational- (core->integer 0) q)
	q)))
)
