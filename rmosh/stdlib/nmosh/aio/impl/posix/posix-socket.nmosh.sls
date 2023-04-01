(library (nmosh aio impl posix posix-socket)
         (export
           queue-listen
           queue-accept
           queue-connect
           resolve-socketname/4
           resolve-socketname/6
           )
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 ;; FIXME; use aliased library for FreeBSD...
                 (nmosh pffi posix fd)
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh pffi posix socket))

;; Socket
;; FIXME: Connect and Listen are not async yet...
(define (queue-connect Q name callback)
  (let ((fd (socket_create 0 1)))
    (socket_connect fd name)
    (callback fd)))

(define (queue-listen Q name callback)
  (let ((fd (socket_create 0 1)))
    (socket_bind fd name)
    (socket_listen fd 5)
    (queue-register-fd/read Q fd (^[fd _] (callback fd)))))

(define (queue-accept Q fd callback)
  (receive (new-fd inetname) (socket_accept fd)
    (callback new-fd inetname)))

;; Resolve API
(define (resolve-socketname**/sync name service mode proto) ;; => (inetname ...)
  (define (addrinfo->list addrinfo)
    (if addrinfo
      (receive (inetname next) (socket_addrinfo_read addrinfo)
        (cons inetname
              (addrinfo->list next)))
      '()))
  (let ((addrinfo (socket_getaddrinfo name service mode proto)))
    (if addrinfo
      (let ((l (addrinfo->list addrinfo)))
        (socket_freeaddrinfo addrinfo) ;; Don't forget to do this..
        l)
      #f)))

;; FIXME:
(define (resolve-socketname** Q name service mode proto cb)
  (cb (resolve-socketname**/sync name service mode proto)))

(define (resolve-socketname/4 Q name service cb)
  (resolve-socketname** Q name service 4 0 cb))

(define (resolve-socketname/6 Q name service cb)
  (resolve-socketname** Q name service 6 0 cb))

#|
(define (resolve-socketname* name service)
  (resolve-socketname** name service 0 0))

(define (resolve-socketname/TCP name service)
  (resolve-socketname** name service 0 1))

(define (resolve-socketname/UDP name service)
  (resolve-socketname** name service 0 2))

(define (resolve-socketname/TCP4 name service)
  (resolve-socketname** name service 4 1))

(define (resolve-socketname/UDP4 name service)
  (resolve-socketname** name service 4 2))

(define (resolve-socketname/TCP6 name service)
  (resolve-socketname** name service 6 1))

(define (resolve-socketname/UDP6 name service)
  (resolve-socketname** name service 6 2))
|#

)
