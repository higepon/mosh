; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Converting R5RS numbers to those of the reference implementation

(define (core->number n)
  (cond
   ((core:exact? n)
    (cond
     ((core:integer? n)
      (core->integer n))
     ((core:rational? n)
      (core->ratnum n))
     ((core:complex? n)
      (core->recnum n))
     (else #f)))
   
   ((core:real? n)
    (core->flonum n))
   (else
    (core->compnum n))))

      
   
		