;; swank-r6rs.sls --- Shareable code between swank-ikarus and swank-larceny
;;
;; Licence: public domain
;; Author: Helmut Eller
;;
;; This is a Swank server barely capable enough to process simple eval
;; requests from Emacs before dying.  No fancy features like
;; backtraces, module redefintion, M-. etc. are implemented.  Don't
;; even think about pc-to-source mapping.
;;
;; Despite standard modules, this file uses (swank os) and (swank sys)
;; which define implementation dependend functionality.  There are
;; multiple modules in this files, which is probably not standardized.
;;

;; CL-style restarts to let us continue after errors.
(library (swank restarts)
    (export with-simple-restart compute-restarts invoke-restart restart-name
	    write-restart-report)
    (import (rnrs))

 (define *restarts* '())

 (define-record-type restart
   (fields name reporter continuation))
 
 (define (with-simple-restart name reporter thunk)
   (call/cc 
    (lambda (k)
      (let ((old-restarts *restarts*)
	    (restart (make-restart name (coerce-to-reporter reporter) k)))
	(dynamic-wind
	    (lambda () (set! *restarts* (cons restart old-restarts)))
	    thunk
	    (lambda () (set! *restarts* old-restarts)))))))

 (define (compute-restarts) *restarts*)

 (define (invoke-restart restart . args)
   (apply (restart-continuation restart) args))

 (define (write-restart-report restart port)
   ((restart-reporter restart) port))

 (define (coerce-to-reporter obj)
   (cond ((string? obj) (lambda (port) (put-string port obj)))
	 (#t (assert (procedure? obj)) obj)))

 )
