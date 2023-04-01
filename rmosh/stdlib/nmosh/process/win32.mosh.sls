(library (nmosh process win32)
         (export process-launch/win32 
                 process-wait/win32
                 process-result/win32
                 process-stdout/win32
                 process-stderr/win32)
         (import (rnrs))

(define (dummy)
  (assertion-violation 'dummy "(nmosh process win32) is nmosh only library!"))

(define process-launch/win32 dummy)
(define process-wait/win32 dummy)
(define process-result/win32 dummy)
(define process-stdout/win32 dummy)
(define process-stderr/win32 dummy)
)

