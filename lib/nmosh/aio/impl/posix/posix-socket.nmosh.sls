(library (nmosh aio impl posix posix-socket)
         (export
           queue-listen
           queue-accept
           queue-connect
           queue-read0
           queue-write0
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

(define (do-read fd callback)
  (let* ((buf (make-bytevector 65536))
         (len (fd_read fd buf 65536)))
    (callback fd buf len)))

(define (queue-read0 Q fd callback)
  (queue-register-fd/read Q fd (^[fd event] (do-read fd callback))))

(define (subbytevector bv off)
  (define len (- (bytevector-length bv) off))
  (define out (make-bytevector len))
  (bytevector-copy! bv off len out 0)
  out)

(define (queue-write0 Q fd data callback)
  ;; Edge trigger
  (define len (bytevector-length data))
  (define off 0)
  (define (proc fd event)
    (let* ((out (subbytevector data off))
           (size (fd_write fd out (bytevector-length out))))
      (assert (<= 0 size))
      (set! off (+ off size))
      (when (= off len)
        (queue-unregister-fd Q fd)
        (callback fd))))
  (cond
    ((= 0 data)
     (callback fd 0))
    (else
      (queue-register-fd/write Q fd proc))))

;; Resolve API
(define (resolve-socketname** name service mode proto) ;; => (inetname ...)
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

(define (resolve-socketname/4 name service)
  (resolve-socketname** name service 4 0))

(define (resolve-socketname/6 name service)
  (resolve-socketname** name service 6 0))

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

)
