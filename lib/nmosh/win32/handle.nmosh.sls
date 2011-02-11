(library (nmosh win32 handle)
         (export 
           win32-handle?
           win32-handle-pipe
           win32-handle-read
           ;win32-handle-write
           win32-handle-close)
         (import (primitives %win32_process_pipe
                             %win32_handle_read
                             %win32_handle_write
                             %win32_handle_close)
                 (mosh ffi)
                 (rnrs))

(define win32-handle? pointer?)

(define (win32-handle-close h)
  (%win32_handle_close h))

(define (win32-handle-read h buf siz)
  (%win32_handle_read h buf siz))

(define (win32-handle-pipe)
  (%win32_process_pipe))

)

