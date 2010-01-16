(library (nmosh startup)
	 (export startup)
	 (import (rnrs) 
		 (rnrs load)
		 (nmosh runlib)
		 (nmosh condition-printer))

(define (do-startup)
  (let ((cl (command-line)))
    (cond
      ((<= 1 (length cl))
       (load (car cl)))
      (else 
	(runlib '((nrepl simple)) 'nrepl)))))

(define (startup)
  (guard
    (e (#t 
	(display "(nmosh startup) unhandled exception." (current-error-port))
	(exit -1)))
    (with-condition-printer/raise (do-startup))))

)
