(import (rnrs)
        (mosh test)
        (mosh shell))
(def-command cp)
(cp "./test/test.txt" "./test/test.txt.temp")

(let ([port (open-file-input/output-port "./test/test.txt.temp" (file-options no-fail no-truncate))])
  (set-port-position! port 4000)
  (let ([bv1 (make-bytevector 10000)])
    (test* (get-bytevector-n! port bv1 0 10000) 10000)
    (put-u8 port #xfc)
    (set-port-position! port 4000)
    (let ([bv (get-bytevector-some port)])
      (display(bytevector-length bv))
      (test* (bytevector-u8-ref bv 0) 123))))


(test-end)
