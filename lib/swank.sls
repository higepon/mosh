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

;; The server proper.  Does the TCP stuff and exception handling.
(library (swank)
    (export start-server)
    (import (rnrs) 
	    (rnrs eval)
	    (swank os)
	    (swank format)
	    (swank event-queue)
	    (swank restarts))

 (define-record-type connection
   (fields in-port out-port event-queue))

 (define (start-server port)
   (accept-connections (or port 4005) #f))

 (define (start-server/port-file port-file)
   (accept-connections #f port-file))

 (define (accept-connections port port-file)
   (let ((sock (make-server-socket port)))
     (printf "Listening on port: ~s\n" (local-port sock))
     (when port-file 
       (write-port-file (local-port sock) port-file))
     (let-values (((in out) (accept sock (latin-1-codec))))
       (dynamic-wind 
	   (lambda () #f)
	   (lambda () 
	     (close-socket sock)
	     (serve in out))
	   (lambda () 
	     (close-port in)
	     (close-port out))))))

 (define (write-port-file port port-file)
   (call-with-output-file 
       (lambda (file) 
	 (write port file))))

 (define (serve in out) 
   (let ((err (current-error-port))
	 (q (make-event-queue 
	     (lambda (q)
	       (let ((e (read-event in)))
		 (printf "read: ~s\n" e)
		 (enqueue-event q e))))))
     (dispatch-loop (make-connection in out q))))

 (define-record-type sldb-state
   (fields level condition continuation next))

 (define (dispatch-loop conn)
   (let ((event (wait-for-event (connection-event-queue conn) 'x)))
     (case (car event)
       ((:emacs-rex) 
	(with-simple-restart 
	 'toplevel "Return to SLIME's toplevel"
	 (lambda ()
	   (apply emacs-rex conn #f (cdr event)))))
       (else (error "Unhandled event: ~s" event))))
   (dispatch-loop conn))

 (define (recover thunk on-error-thunk)
   (let ((ok #f))
     (dynamic-wind 
	 (lambda () #f) 
	 (lambda () 
	   (call-with-values thunk 
	     (lambda vals 
	       (set! ok #t) 
	       (apply values vals))))
	 (lambda ()
	   (unless ok
	     (on-error-thunk))))))

 ;; Couldn't resist to exploit the prefix feature.
 (define rpc-entries (environment '(prefix (swank rpc) swank:)))
 
 (define (emacs-rex conn sldb-state form package thread tag)
   (let ((out (connection-out-port conn)))
     (recover
      (lambda ()
	(with-exception-handler
	 (lambda (condition) 
	   (call/cc 
	    (lambda (k)
	      (sldb-exception-handler conn condition k sldb-state))))
	 (lambda ()
	   (let ((value (apply (eval (car form) rpc-entries) (cdr form))))
	     (write-event `(:return (:ok ,value) ,tag) out)))))
      (lambda ()
	(write-event `(:return (:abort) ,tag) out)))))

 (define (sldb-exception-handler connection condition k sldb-state)
   (when (serious-condition? condition)
     (let ((level (if sldb-state (+ (sldb-state-level sldb-state) 1) 1))
	   (out (connection-out-port connection)))
       (write-event `(:debug 0 ,level ,@(debugger-info condition connection))
		    out)
       (dynamic-wind
	   (lambda () #f)
	   (lambda ()
	     (sldb-loop connection 
			(make-sldb-state level condition k sldb-state)))
	   (lambda () (write-event `(:debug-return 0 ,level nil) out))))))

 (define (sldb-loop connection state)
   (apply emacs-rex connection state
	  (cdr (wait-for-event (connection-event-queue connection) 
			       '(':emacs-rex . _))))
   (sldb-loop connection state))

 (define (debugger-info condition connection)
   (list `(,(call-with-string-output-port 
	     (lambda (port) (print-condition condition port)))
	   ,(format " [type ~s]" (if (record? condition)
				     (record-type-name (record-rtd condition))
				     ))
	   ())
	 (map (lambda (r) 
		(list (format "~a" (restart-name r))
		      (call-with-string-output-port
		       (lambda (port)
			 (write-restart-report r port)))))
	      (compute-restarts))
	 '()
	 '()))

 (define (print-condition obj port)
   (cond ((condition? obj)
	  (let ((list (simple-conditions obj)))
	    (case (length list)
	      ((0)
	       (display "Compuond condition with zero components" port))
	      ((1)
	       (assert (eq? obj (car list)))
	       (print-simple-condition (car list) port))
	      (else
	       (display "Compound condition:\n" port)
	       (for-each (lambda (c)
			   (display "  " port)
			   (print-simple-condition c port)
			   (newline port))
			 list)))))
	 (#t
	  (fprintf port "Non-condition object: ~s" obj))))

 (define (print-simple-condition condition port)
   (fprintf port "~a" (record-type-name (record-rtd condition)))
   (case (count-record-fields condition)
     ((0) #f)
     ((1) 
      (fprintf port ": ")
      (do-record-fields condition (lambda (name value) (write value port))))
     (else
      (fprintf port ":")
      (do-record-fields condition (lambda (name value) 
				    (fprintf port "\n~a: ~s" name value))))))

 ;; Call FUN with RECORD's rtd and parent rtds.
 (define (do-record-rtds record fun)
   (do ((rtd (record-rtd record) (record-type-parent rtd)))
       ((not rtd))
     (fun rtd)))

 ;; Call FUN with RECORD's field names and values.
 (define (do-record-fields record fun)
   (do-record-rtds 
    record
    (lambda (rtd)
      (let* ((names (record-type-field-names rtd))
	     (len (vector-length names)))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (fun (vector-ref names i) ((record-accessor rtd i) record)))))))

 ;; Return the number of fields in RECORD
 (define (count-record-fields record)
   (let ((i 0))
     (do-record-rtds 
      record (lambda (rtd) 
	       (set! i (+ i (vector-length (record-type-field-names rtd))))))
     i))

 )
