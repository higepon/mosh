(library (yuni compat simple-struct)
         (export make-simple-struct
                 simple-struct-name
                 simple-struct-ref
                 simple-struct-set!
                 simple-struct?)
         (import (rnrs)
                 (primitives make-simple-struct
                             simple-struct-name
                             simple-struct?
                             simple-struct-set!
                             simple-struct-ref)))
