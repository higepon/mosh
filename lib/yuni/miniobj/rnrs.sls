(library (yuni miniobj rnrs)
         (export miniobj-rnrs-ref 
                 miniobj-rnrs-set!
                 miniobj-rnrs-ref-error
                 miniobj-rnrs-set!-error
                 )
         (import (rnrs))

(define (miniobj-rnrs-set! obj slot value k)
  (cond
    ((hashtable? obj)
     (hashtable-set! obj slot value))
    ((vector? obj)
     (vector-set! obj slot value))
    (else (k obj slot value))))

(define (miniobj-rnrs-ref obj slot k)
  (cond
    ((hashtable? obj)
     (hashtable-ref obj slot #f))
    ((vector? obj)
     (vector-ref obj slot))
    ((pair? obj)
     (list-ref obj slot))
    (else (k obj slot))))

(define (miniobj-rnrs-ref-error obj slot)
  (assertion-violation 'miniobj-ref "unsupported object" (list obj slot)))
(define (miniobj-rnrs-set!-error obj slot value)
  (assertion-violation 'miniobj-set! "unsupported object" (list obj slot value)))

)
