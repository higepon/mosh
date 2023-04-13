(import (rnrs)
        (shorten)
        (nmosh io core)
        (nmosh io tcp0)
        (nmosh net msgpack))

(define (err obj) 
  (display "server socket lost.\n"))

(define (starter fd inetname)
  (define (err obj) 
    (display "client connection lost.\n")
    ;(socket-close fd)
    )
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
