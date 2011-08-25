(library (nmosh pffi interface)
         (export 
           pointer?
           pointer->integer
           integer->pointer
           object->pointer
           pointer->object
           bytevector-pointer
           pointer-copy!)
         (import (rnrs)
                 (mosh ffi)
                 (primitives bytevector-pointer
                             pointer-copy!
                             object->pointer
                             pointer->object
                             ))
)
