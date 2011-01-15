#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (yuni lib ssax private output)
  (export
    cout cerr nl)
  (import
    (rnrs))


; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args)
  (newline))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
		  (x (current-error-port))
		  (display x (current-error-port))))
            args)
  (newline (current-error-port)))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))

)
