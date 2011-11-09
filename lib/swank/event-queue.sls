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

;; This module encodes & decodes messages from the wire and queues them.
(library (swank event-queue)
    (export make-event-queue wait-for-event enqueue-event 
	    read-event write-event)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (swank format))

 (define-record-type event-queue
   (fields (mutable q) wait-fun)
   (protocol (lambda (init)
	       (lambda (wait-fun)
		 (init '() wait-fun)))))

 (define (wait-for-event q pattern)
   (or (poll q pattern)
       (begin
	 ((event-queue-wait-fun q) q)
	 (wait-for-event q pattern))))
 
 (define (poll q pattern)
   (let loop ((lag #f)
	      (l (event-queue-q q)))
     (cond ((null? l) #f)
	   ((event-match? (car l) pattern)
	    (cond (lag 
		   (set-cdr! lag (cdr l))
		   (car l))
		  (else
		   (event-queue-q-set! q (cdr l))
		   (car l))))
	   (else (loop l (cdr l))))))

 (define (event-match? event pattern)
   (cond ((or (number? pattern)
	      (member pattern '(t nil)))
	  (equal? event pattern))
	 ((symbol? pattern) #t)
	 ((pair? pattern)
	  (case (car pattern)
	    ((quote) (equal? event (cadr pattern)))
	    ((or) (exists (lambda (p) (event-match? event p)) (cdr pattern)))
	    (else (and (pair? event)
		       (event-match? (car event) (car pattern))
		       (event-match? (cdr event) (cdr pattern))))))
	 (else (error "Invalid pattern: " pattern))))
 
 (define (enqueue-event q event)
   (event-queue-q-set! q
		       (append (event-queue-q q) 
			       (list event))))

 (define (write-event event port)
   (let ((payload (call-with-string-output-port
		   (lambda (port) (write event port)))))
     (write-length (string-length payload) port)
     (put-string port payload)
     (flush-output-port port)))

 (define (write-length len port)
   (do ((i 24 (- i 4)))
       ((= i 0))
     (put-string port
		 (number->string (bitwise-bit-field len (- i 4) i)
				 16))))

;; add
(define (check-for-eof s)
  (when (eof-object? s) (exit))
  s)

 (define (read-event port)
   (let* ((header (string-append (check-for-eof (get-string-n port 2)) 
				 (check-for-eof (get-string-n port 2)) 
				 (check-for-eof (get-string-n port 2))))
	  (_ (printf "header: ~s\n" header))
	  (len (string->number header 16))
	  (_ (printf "len: ~s\n" len))
	  (payload (get-string-n port len)))
     (printf "payload: ~s\n" payload)
     (read (open-string-input-port payload))))

 )
