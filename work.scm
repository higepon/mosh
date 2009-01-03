(import (rnrs))

(define cont #f)

(define (f)
  (call/cc
   (lambda (break)
     (call-with-port (open-string-input-port "abc")
       (lambda (p)
	 (let loop ((c (read-char p)))
	   (cond ((eof-object? c)
		  (newline))
		 (else
		  (display c)
		  (call/cc
		   (lambda (k)
		     (set! cont k)
		     (break)))
		  (loop (read-char p))))))))))
(f)
(cont)
(cont)
(cont)
