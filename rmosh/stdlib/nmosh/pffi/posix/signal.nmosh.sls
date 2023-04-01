(library (nmosh pffi posix signal)
         (export signal)
         (import (rnrs)
                 (mosh))

(define-syntax signal
  (syntax-rules ()
    ((_ sym)
     (os-constant 'sym))))

)
