(library (nmosh pffi win32 locale)
         (export host-string->string)
         (import 
           (rnrs)
           (nmosh pffi win32 util))

(define host-string->string mbcs->string)

)
