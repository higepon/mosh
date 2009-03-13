(import (rnrs)
        (mosh test))


(let* ([save #f]
           [p (make-custom-binary-input/output-port
               "custom in"
               (lambda (bv start end)
                 (bytevector-u8-set! bv start 7)
                 1)
               (lambda (bv start end)
                 (set! save (bytevector-u8-ref bv start))
                 1)
               #f #f #f)])
      (put-u8 p 10)
      (flush-output-port p)
      (test* save 10)
      (test* (get-u8 p) 7)
      (close-port p))

(test-end)
