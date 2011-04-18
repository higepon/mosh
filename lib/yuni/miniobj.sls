(library (yuni miniobj)
         (export miniobj-ref
                 miniobj-set!
                 miniobj-typeof)
         (import (rnrs)
                 (yuni miniobj rnrs)
                 (yuni miniobj minitype)
                 (yuni miniobj base))

(define-miniobj-typeof miniobj-typeof
                       miniobj-minitype-typeof
                       miniobj-minitype-typeof-error)

(define-miniobj-ref miniobj-ref
                    miniobj-minitype-ref
                    miniobj-rnrs-ref
                    miniobj-rnrs-ref-error)

(define-miniobj-set! miniobj-set!
                     miniobj-minitype-set!
                     miniobj-rnrs-set!
                     miniobj-rnrs-set!-error)

)
