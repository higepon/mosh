(library (nmosh gui win32)
         (export errorbox)
         (import (rnrs)
                 (nmosh win32 gui))
(define errorbox
  (case-lambda
    ((msg) (win32_messagebox "nmosh error" msg 0 2))
    ((caption msg) (win32_messagebox caption msg 0 2))))
)

