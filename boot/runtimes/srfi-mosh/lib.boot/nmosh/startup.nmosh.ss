(library (nmosh startup)
	 (export startup)
	 (import (rnrs) 
		 (nmosh runlib)
		 (nmosh condition-printer)
		 (nmosh minidebug)
		 (primitives ca-load ca-load/disable-cache set-symbol-value!))

(define (startup)
  (set-symbol-value! '%nmosh-failproc enter-debugger)
  (let ((cl (command-line)))
    (cond
      ((<= 1 (length cl))
       (ca-load (car cl) #f '(nmosh PROGRAM-FROM-NMOSH-STARTUP)))
      (else 
	(runlib '((nrepl simple)) 'nrepl)))))

(define (enter-debugger c trace)
  (define (fallback x)
    (display "debugger not found.\n" (current-error-port))
    (call-with-port (current-error-port) 
		    (lambda (p) (minidebug p c trace))))
  (display "launching debugger...\n" (current-error-port))
  (runlib/fallback fallback '((nmosh debugger)) 'debugger c trace))


)
