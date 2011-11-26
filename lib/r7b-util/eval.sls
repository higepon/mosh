;; FAKE
#!r6rs
(library (r7b-util eval)
         (export interaction-environment
                 load
                 environment
                 eval
                 null-environment scheme-report-environment)
         (import (rnrs))

(define interaction-environment 0)
(define load 0)
(define environment 0)
(define eval 0)
(define null-environment 0)
(define scheme-report-environment 0)

)
