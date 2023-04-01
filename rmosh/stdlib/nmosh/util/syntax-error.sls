(library (nmosh util syntax-error)
         (export syntax-error)
         (import (rnrs))
;; FIXME: follow R7RS spec
(define (syntax-error . arg)
  (assertion-violation 'syntax-error
                       "syntax error"
                       arg))

)
