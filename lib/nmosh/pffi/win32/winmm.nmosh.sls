(library (nmosh pffi win32 winmm)
         (export winmm-joy-count
                 winmm-joy-read)
         (import (rnrs)
                 (yuni binary macro packet0)
                 (nmosh pffi interface)
                 (nmosh stubs mosh-winmm))

(define-packet0*
  winmm-joydataex pack-winmm-joydataex unpack-winmm-joydataex
  (dwSize "unsigned" 4 little)
  (dwFlags "unsigned" 4 little)
  (dwXpos "unsigned" 4 little)
  (dwYpos "unsigned" 4 little)
  (dwZpos "unsigned" 4 little)
  (dwRpos "unsigned" 4 little)
  (dwUpos "unsigned" 4 little)
  (dwVpos "unsigned" 4 little)
  (dwButtons "unsigned" 4 little)
  (dwButtonNumber "unsigned" 4 little)
  (dwPOV "unsigned" 4 little)
  (dwReserved1 "unsigned" 4 little)
  (dwReserved2 "unsigned" 4 little)
  )

(define (winmm-joy-count) (mmm_joy_count))
(define (winmm-joy-read id) ;; => joydata / #f
  (define bv (make-bytevector (* 13 4)))
  (let ((res (mmm_joy_read id bv)))
    (and (= res 1)
         (unpack-winmm-joydataex bv 0))))

)
