(import (rnrs)
        (mosh test)
        (mosh shell))

(def-command cp)
(cp "./test/test.txt" "./test/test.txt.temp")

(let ([port (open-file-output-port "./test/test.txt.temp")])
    (set-port-position! port 4000)
    (put-bytevector port (make-bytevector 9000 #x13))
    (close-port port))


(let ([port (open-file-input-port "./test/test.txt.temp")])
    (set-port-position! port 4000)
    (let ([bv (get-bytevector-n port 9000)])
      (display bv)
      (test/t (for-all (lambda (x) (= #x13 x)) (bytevector->u8-list bv)))))
(test-end)
