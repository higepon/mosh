(library (nmosh pffi win32 env)
         (export setenv)
         (import (rnrs)
                 (nmosh pffi win32 util)
                 (prefix (nmosh stubs win32-misc)
                         stub:))

(define (setenv var val)
  (stub:win32_setenv (string->utf16-bv var)
                     (string->utf16-bv val)))

)
