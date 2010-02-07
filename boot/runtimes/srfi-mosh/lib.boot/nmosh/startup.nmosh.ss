(library (nmosh startup)
	 (export startup)
	 (import (rnrs) 
		 (rnrs load)
		 (nmosh runlib)
		 (nmosh condition-printer)
		 (nmosh minidebug)
		 (primitives ca-load set-symbol-value!))

(define (startup)
  (let ((cl (command-line)))
    (cond
      ((<= 1 (length cl))
       ;(ca-load (car cl) #f 'STARTUP-PROGRAM))
       (load (car cl)))
      (else 
	(runlib '((nrepl simple)) 'nrepl)))))

(define (enter-debugger c trace)
  (define (fallback x)
    (display "debugger not found.\n" (current-error-port))
    (call-with-port (current-error-port) 
		    (lambda (p) (minidebug p c trace))))
  (display "launching debugger...\n" (current-error-port))
  (runlib/fallback fallback '((nmosh debugger)) 'debugger c trace))


(set-symbol-value! '%nmosh-failproc enter-debugger)
)
