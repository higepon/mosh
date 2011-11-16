(library (nmosh pffi posix socket)
         (export 
           ;; socket
           socket_getaddrinfo
           socket_create
           socket_freeaddrinfo
           socket_bind
           socket_listen
           socket_connect
           socket_accept
           socket_addrinfo_read
           socket_setnodelay
           )
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (system)
                 (srfi :8)
                 (prefix (nmosh stubs posix-socket) stub:))


(define null-pointer (integer->pointer 0))
(define (null-pointer? x) (= (pointer->integer x) 0))

;; socket

(define* addrinfo (ptr))

(define (pointer->addrinfo ptr)
  (make addrinfo (ptr ptr)))
(define* (addrinfo->pointer (addrinfo))
  (let-with addrinfo (ptr) ptr))

(define* inetname (name len))

(define (make-inetname ptr len)
  (let* ((bv (make-bytevector len))
         (bv-ptr (bytevector-pointer bv)))
    (pointer-copy! ptr bv-ptr len)
    (make inetname 
      (name bv)
      (len len))))

(define (make-inetname/bytevector bv len)
  (make inetname 
    (name bv)
    (len len)))

(define* (inetname-values (inetname))
  (let-with inetname (name len) (values name len)))

(define* (inetname-name (inetname))
  (receive (name len) (inetname-values inetname)
    name))

(define* (inetname-len (inetname))
  (receive (name len) (inetname-values inetname)
    len))

;; mode 0/4/6, proto 0/1(TCP)/2(UDP)
(define (socket_getaddrinfo name service mode proto)
  (let ((ret (make-ptr-box))
        (service (if (number? service) (number->string service) service)))
    (let ((r (stub:socket_getaddrinfo name service ret mode proto)))
      ;; FIXME: handle error
      (if (= 0 r)
        (pointer->addrinfo (ptr-box-ref ret))
        #f))))

(define* (socket_create mode proto)
  (let ((r (stub:socket_create mode proto)))
    (if (< 0 r)
      (int->fd r)
      #f)))

(define* (socket_freeaddrinfo (addrinfo))
  (stub:socket_freeaddrinfo (addrinfo->pointer addrinfo)))

(define* (socket_bind (fd) (inetname))
  (receive (name len) (inetname-values inetname)
    (stub:socket_bind (fd->int fd)
                      name
                      len)))

(define* (socket_listen (fd) n)
  (stub:socket_listen (fd->int fd) n))

(define* (socket_connect (fd) (inetname))
  (receive (name len) (inetname-values inetname)
    (stub:socket_connect (fd->int fd)
                         name
                         len)))

(define* (socket_accept (fd)) ;; => fd inetname / #f
  (let* ((len (stub:socket_sizeof_sockaddr_storage))
         (bv (make-bytevector len))
         (ret-box (make-int-box)))
    (int-box-set! ret-box len)
    (let ((r (stub:socket_accept (fd->int fd)
                                 bv
                                 ret-box)))
      (if (< r 1)
        (values #f #f)
        (values (int->fd r)
                (make-inetname/bytevector bv len))))))

(define* (socket_addrinfo_read (addrinfo)) ;; => inetname addrinfo/#f
  (let ((box-addr (make-ptr-box))
        (box-len (make-int-box))
        (box-next (make-ptr-box))
        (box-family (make-int-box)))
    (stub:socket_addrinfo_read (addrinfo->pointer addrinfo)
                               box-family
                               box-addr
                               box-len
                               box-next)
    (let ((addr (ptr-box-ref box-addr))
          (len (int-box-ref box-len))
          (next (ptr-box-ref box-next))
          (family (int-box-ref box-family)))
      (values
        (if (null-pointer? addr) #f (make-inetname addr len))
        (if (null-pointer? next) #f (pointer->addrinfo next))))))

(define* (socket_setnodelay (fd))
  (stub:socket_setnodelay (fd->int fd)))

)
