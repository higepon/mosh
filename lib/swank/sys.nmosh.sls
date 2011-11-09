;; psyntax-mosh version
(library (swank sys)
         (export implementation-name eval-in-interaction-environment)
         (import (rnrs)
                 (mosh pp)
                 (primitives
                   ex:destructive-eval!
                   ex:interaction-environment))
(define (implementation-name) "nmosh")
(define (eval-in-interaction-environment form)
  (pp form)
  (ex:destructive-eval! form (ex:interaction-environment)))
)

