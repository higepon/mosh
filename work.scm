(import (rnrs)
 (mosh ffi))
(define self (open-shared-library ""))
(define platform-malloc
 (make-c-function self 'void* 'malloc '(int)))
(define p (platform-malloc (expt 10 5)))
(pointer-set-c-char! p 100 2000)
