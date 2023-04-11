(import (rnrs)
        (srfi :8)
        (mosh test)
        (primitives
          set-current-error-port!
          set-current-output-port!))

;; Test for output port
(receive (port proc) (open-string-output-port)
  (define oldp (current-output-port))
  (set-current-output-port! port)
  (display "hoge fuga")
  (set-current-output-port! oldp)
  (test-equal "hoge fuga" (proc))
  (close-port port))

;; Test for error port
(receive (port proc) (open-string-output-port)
  (define oldp (current-error-port))
  (set-current-error-port! port)
  (display "hoge fuga" (current-error-port))
  (set-current-error-port! oldp)
  (test-equal "hoge fuga" (proc))
  (close-port port))

(test-results)
