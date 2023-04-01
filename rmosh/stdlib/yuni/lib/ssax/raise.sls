#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (yuni lib ssax raise)
  (export
    parser-error
    ssax:warn)
  (import
    (rnrs))
  
  (define-condition-type &port-position &condition
    make-port-position-condition port-position-condition?
    (pos condition-port-position))
  
  (define (make-f raise-it first who)
    (lambda (port msg . other-msg)
      (raise-it
       (condition
        first
        (make-who-condition who)
        (make-message-condition (call-with-string-output-port
                                 (lambda (sop)
                                   (for-each (lambda (x) (display x sop))
                                             (cons msg other-msg)))))
        (if (and (port? port) (port-has-port-position? port))
          (make-port-position-condition (port-position port))
          (condition))
        (make-irritants-condition (list port))))))
   
  (define parser-error
    (make-f raise (make-error) 'ssax:parser))
   
  (define ssax:warn
    ;; None of these condition types are &serious,
    ;; so a default exception handler should return (per the R6RS),
    ;; allowing the SSAX code which called this to continue.
    (make-f raise-continuable (make-warning) 'ssax))
  
)
