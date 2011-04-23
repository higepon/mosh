(library (nmosh win32 gui)
         (export win32_messagebox)
         (import (rnrs)
                 (nmosh win32 util)
                 (prefix (nmosh stubs win32-gui) stub:))
(define (win32_messagebox caption msg dlgtype icontype)
  (stub:win32_messagebox 
    (string->utf16-bv caption)
    (string->utf16-bv msg)
    dlgtype
    icontype))
)
