(import (rnrs) (nmosh mosh-vm) (mosh pp))

(define (readit)
  (call-with-port (open-file-input-port "nmosh.nmosh-dbg")
		  (lambda (p)
		    (let ((bv (get-bytevector-all p)))
		      (fasl->obj bv)))))

(let ((d (readit)))
  (define (nstep e)
    (pp e))
  (define (step e)
    (for-each nstep e))
  (for-each step d))
