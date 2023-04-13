(import (rnrs)
        (shorten)
        (match)
        (nmosh io core)
        (nmosh net msgpack))

(define (err obj) 
  (display (list 'DISCONNECTED: obj))(newline)
  (exit -1))

(define count 0)
(define vcount 0)
(define latest #f)

(define (calcsize x)
  ;(display (list 'calc: x))(newline)
  (+ 4096
     (mod x 903)) )

(define (gen)
  (set! count (+ count 1))
  (when (= (mod count 100) 0)
    (display (list 'SEND: count 'VERIFY: vcount (list latest)))(newline))
  `(,count ,(number->string count) ,(make-bytevector (calcsize count))))

(define (verify obj)
  (set! vcount (+ vcount 1))
  (match obj
         ((num dat obj)
          (assert (= num (string->number (utf8->string dat))))
          (assert (= (calcsize num) (bytevector-length obj)))
          (set! latest num))))

(define (start-writer output)
  (define (next)
    (output (gen) next))
  (next))

(make-msgpack-client-socket
  "127.0.0.1"
  9988
  (^[obj]
    (verify obj))
  (^[proc]
    (start-writer proc))
  err)

(io-dispatch-loop)
