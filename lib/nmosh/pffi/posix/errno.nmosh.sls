(library (nmosh pffi posix errno)
         (export errno)
         (import (rnrs)
                 (mosh))
(define-syntax errno
  (syntax-rules ()
    ((_ sym)
     (os-constant 'sym))))
)
