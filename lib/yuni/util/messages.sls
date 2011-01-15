(library (yuni util messages)
         (export info
                 warn
                 err)
         (import (rnrs)
                 (srfi :48))
(define (mesg . args)
  (display "--> " (current-error-port))
  (apply format (cons (current-error-port)
                      args))
  (newline (current-error-port)))

(define info mesg)
(define warn mesg)
(define err mesg)

)
