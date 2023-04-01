(library (swank os)
         (export getpid make-server-socket accept local-port close-socket)
         (import (rnrs)
                 (mosh process) ;; getpid
                 (rename (mosh socket) ;; make-server-socket
                         (make-server-socket mosh:make-server-socket))
                 )

;; global hashtable for sockets
(define ht-port (make-eq-hashtable))

(define (make-server-socket port)
  (let ((s (mosh:make-server-socket (number->string port))))
    (hashtable-set! ht-port s port)
    s))

(define (accept sock codec)
  (let ((p (transcoded-port (socket-port (socket-accept sock)) (make-transcoder codec))))
    (values p p)))

(define (local-port sock)
  (or (hashtable-ref ht-port sock #f)
      (assertion-violation 'local-port
                           "socket not found"
                           sock)))

(define (close-socket sock)
  (hashtable-delete! ht-port sock)
  (socket-close sock))

)
