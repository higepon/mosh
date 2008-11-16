(import (rnrs)
        (mosh string))

(let ([b (make-bytevector 8)])
  (bytevector-s64-set! b 0 -144115188075855873 'little)
                                         b)
(display (bytevector-ieee-single-native-ref #vu8(#xc3 #xf5 #x48 #x40) 0))
