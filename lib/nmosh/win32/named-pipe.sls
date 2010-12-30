(library (nmosh win32 named-pipe)
         (export win32-named-pipe-create
                 win32-named-pipe-wait)
         (import 
           (rnrs)
           (nmosh win32 util)
           (primitives %win32_named_pipe_wait %win32_named_pipe_create))

(define (win32-named-pipe-create name)
  (let ((h (%win32_named_pipe_create (string->utf16-bv name))))
    h))

(define (win32-named-pipe-wait h)
  (%win32_named_pipe_wait h))

)
