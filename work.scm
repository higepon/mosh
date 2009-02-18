(import (rnrs)
        (mosh))

(let* ([pos 0]
       [p (make-custom-binary-input-port
           "custom in"
           (lambda (bv start count)
             (if (= pos 16)
                 0
                 (begin
                   (set! pos (+ 1 pos))
                   (bytevector-u8-set! bv start pos)
                   1)))
           (lambda () pos)
           (lambda (p) (set! pos p))
           (lambda () 'ok))])
  (test/t (port-has-port-position? p))
  (test/t (port-has-set-port-position!? p))
  (test* (port-position p) 0)
  (test* (get-bytevector-n p 3) #vu8(1 2 3))
  (test* (port-position p) 3)
  (test* (lookahead-u8 p) 4)
  (test* (lookahead-u8 p) 4)
  (test* (port-position p) 3)
  (set-port-position! p 10)
  (get-bytevector-n p 2)
  (test* (get-bytevector-n p 2) #vu8(13 14))
  (test* (get-bytevector-n p 2) #vu8(15 16))
  (test* (get-bytevector-n p 2) (eof-object))
  (set-port-position! p 2)
  (test* (get-bytevector-n p 3) #vu8(3 4 5))
  (close-port p))
