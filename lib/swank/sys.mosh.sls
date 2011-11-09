;; psyntax-mosh version
(library (swank sys)
         (export implementation-name eval-in-interaction-environment)
         (import (rnrs)
                 (rnrs eval)
                 (only (psyntax system $all) interaction-environment))
(define (implementation-name) "mosh")
(define (eval-in-interaction-environment form)
  (eval form (interaction-environment)))
)

