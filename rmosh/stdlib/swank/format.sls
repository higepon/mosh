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

;; Naive FORMAT implementation which supports: ~a ~s ~d ~x ~c
(library (swank format)
    (export format printf fprintf)
    (import (rnrs))

 (define (format f . args)
   (call-with-string-output-port
    (lambda (port) (apply fprintf port f args))))

 (define (printf f . args)
   (let ((port (current-output-port)))
     (apply fprintf port f args)
     (flush-output-port port)))

 (define (fprintf port f . args)
   (let ((len (string-length f)))
     (let loop ((i 0) (args args))
       (cond ((= i len) (assert (null? args)))
	     ((and (char=? (string-ref f i) #\~)
		   (< (+ i 1) len))
	      (dispatch-format (string-ref f (+ i 1)) port (car args))
	      (loop (+ i 2) (cdr args)))
	     (else
	      (put-char port (string-ref f i))
	      (loop (+ i 1) args))))))
 
 (define (dispatch-format char port arg)
   (let ((probe (assoc char format-dispatch-table)))
     (cond (probe ((cdr probe) arg port))
	   (else (error "invalid format char: " char)))))

 (define format-dispatch-table 
   `((#\a . ,display)
     (#\s . ,write)
     (#\d . ,(lambda (arg port) (put-string port (number->string arg 10))))
     (#\x . ,(lambda (arg port) (put-string port (number->string arg 16))))
     (#\c . ,(lambda (arg port) (put-char port arg))))))
