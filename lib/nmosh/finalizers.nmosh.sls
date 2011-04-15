(library (nmosh finalizers)
         (export make-finalizer
                 register-finalizer)
         (import (mosh ffi)
                 (yuni core)
                 (rnrs)
                 (primitives object->pointer)
                 (prefix (nmosh stubs boehmgc-stubs) stub:))

(define* finalizer (pointer))

(define (make-finalizer pointer)
  (unless (pointer? pointer)
    (assertion-violation 'make-finalizer
                         "invalid argument"
                         pointer))
  (make finalizer (pointer pointer)))

(define* (register-finalizer obj (f finalizer) user-data)
  (let-with f (pointer)
    (stub:register_finalizer (object->pointer obj)
                             pointer
                             user-data)))

)
