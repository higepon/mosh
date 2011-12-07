(library (nmosh aio impl win32 socket-ops)
         (export 
           ;; TCP
           queue-connect
           queue-listen
           queue-accept
           resolve-socketname/4
           resolve-socketname/6
           )
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (nmosh pffi win32 aio)
                 (nmosh pffi interface)
                 (nmosh aio impl win32 queue-iocp)
                 (nmosh aio impl win32 handle-ops))

(define* inetname (family sockaddr len))

(define connectex)
(define acceptex)

;; proto = 1 .. TCP, 2 .. UDP
(define (socket proto)
  (receive (sock sock-connectex sock-acceptex) (win32_socket_create 4 proto)
    (when sock-connectex
      (set! connectex sock-connectex))
    (when sock-acceptex
      (set! acceptex sock-acceptex))
    sock))

(define (socket/TCP) (socket 1))
(define (socket/UDP) (socket 2))

(define (queue-connect Q name callback)
  (let* ((sock (socket/TCP))
         (str (handle->stream Q sock))
         (my-ovl (win32_overlapped_alloc)))
    (define (connected err bytes ovl key)
      (win32_overlapped_free my-ovl)
      (callback str))
    (win32_overlapped_setmydata my-ovl (object->pointer connected))
    (queue-register-handle Q sock str)
    (let-with name (sockaddr len)
      (win32_socket_connect connectex sock sockaddr len my-ovl))))


(define* listen-socket (ovl callback buf accept-sock))


(define BLKSIZE (* 64 1024))

(define* (queue-listen Q (inetname) callback)
  (define my-ovl (win32_overlapped_alloc))
  (define sockaddr-size (+ 16 (win32_sockaddr_storage_size)))
  (define listen-sock (socket/TCP))
  (define listen (make listen-socket 
                       (ovl my-ovl)
                       (accept-sock #f)
                       (callback callback)))
  (define (enqueue-action)
    (define accept-sock (socket/TCP))
    (define accept-buffer (make-bytevector BLKSIZE))
    (~ listen 'accept-sock := accept-sock)
    ;; Enqueue accept action
    (win32_socket_accept acceptex listen-sock accept-sock 
                         accept-buffer
                         (* 2 sockaddr-size) ;; Do not perform receive action
                         my-ovl))

  (define (listen-callback err bytes ovl key)
    (callback listen)
    (enqueue-action))

  (let-with inetname (sockaddr len) 
    ;(display (list 'BIND: sockaddr len))(newline)
    ;; FIXME: check errors
    (win32_socket_bind listen-sock sockaddr len)
    (win32_socket_listen listen-sock))
  ;; Register callback
  (win32_overlapped_setmydata my-ovl (object->pointer listen-callback))
  (queue-register-handle Q listen-sock listen)
  (enqueue-action))

(define (queue-accept Q fd callback)
  ;(display (list 'ACCEPT fd callback))(newline)
  (callback (handle->stream Q (~ fd 'accept-sock)) 'BOGUS))

(define (capture ptr len)
  (define bv (make-bytevector len))
  (pointer-copy! ptr (bytevector-pointer bv) len)
  bv)

(define (addrinfo-read-step addrinfo) ;; (inetname next/#f)
  (receive (family addr namelen next) (win32_addrinfoex_read addrinfo)
    ;(display (list 'ADDR: addr namelen))(newline)
    (values
      (make inetname
            (family family)
            (sockaddr (capture addr namelen))
            (len namelen))
      next)))

;; Resolve API
(define (resolve-socketname**/sync name service mode proto) ;; => (inetname ...)
  (define (addrinfo->list addrinfo)
    (if addrinfo
      (receive (inetname next) (addrinfo-read-step addrinfo)
        (cons inetname
              (addrinfo->list next)))
      '()))
  (let ((addrinfo (win32_getaddrinfo name service mode proto)))
    (if addrinfo
      (let ((l (addrinfo->list addrinfo)))
        (win32_addrinfoex_free addrinfo) ;; Don't forget to do this..
        l)
      #f)))

;; FIXME:
(define (resolve-socketname** Q name service mode proto cb)
  (cb (resolve-socketname**/sync name service mode proto)))

;; FIXME: TCP only
(define (resolve-socketname/4 Q name service cb)
  (resolve-socketname** Q name service 4 1 cb))

(define (resolve-socketname/6 Q name service cb)
  (resolve-socketname** Q name service 6 1 cb))


)
