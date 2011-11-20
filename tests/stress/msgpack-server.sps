(import (rnrs)
        (shorten)
        (nmosh io core)
        (nmosh net msgpack))

(define (err obj) (assert #f))

(define (starter fd inetname)
  (define writer #f)
  (define (write-callback proc)
    (set! writer proc))
  (define (recv obj)
    ;(display (list 'RECV: obj))(newline)
    (cond
      (writer 
        (writer obj (^[] 'ok)))
      (else (display (list 'DROP obj))(newline))))
  (start-msgpack-talker fd recv write-callback err))

(make-msgpack-server-socket
  "127.0.0.1"
  9988
  starter
  err)

(display "start...")(newline)
(io-dispatch-loop)
