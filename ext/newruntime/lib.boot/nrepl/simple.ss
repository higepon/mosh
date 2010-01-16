(library (nrepl simple)
	 (export nrepl)
	 (import (rnrs)
		 (only (mosh) format )
		 (nmosh condition-printer)
		 (primitives nm:eval-str exit)) ;;MOSH only..

;-------------------------------------------------------------------------
; simple REPL
;-------------------------------------------------------------------------
	 (define base-eval nm:eval-str)
	 (define (display-banner)
	   (display "nmosh top program")(newline))

	 (define (display-prompt)
	   (display "nmosh> "))
	 
	 (define (read-one)
	   (display-prompt)
	   (let ((l (get-line (current-input-port))))
	     (when (eof-object? l) (exit))
	     (call-with-port (open-string-input-port l) read)))

	 (define (convstr l)
	   (call-with-string-output-port (lambda (p) (write l p))))

	 (define (do-eval l)
	   (base-eval (convstr (list l))))

	 (define (eval-one)
	   (let ((l (read-one)))
	     (cond
	       ((not (list? l)) (display "?form")(newline))
	       (else
		 (do-eval l))))) 

	 (define (loop)
	   (guard (e
		    (#t ;always handle
		     (if (condition? e)
		       (condition-printer e (current-error-port))
		       (display (list 'UNKNOWN-DATUM! e)))))
		  (eval-one))
	   (loop))

	 (define (nrepl)
	   (do-eval '(import (rnrs)))
	   (display-banner)
	   (loop))
)

