(library (nmosh bsd kqueue)
         (export 
           kq_create
           kevent_alloc
           kevent_dispose
           kevent_set_readevent!
           kevent_set_writeevent!
           kevent_set_enableuserevent!
           kevent_set_triggeruserevent!
           kevent_ident
           kevent_type
           kevent_exec
           kevent_decode_fd

           ;; socket
           socket_getaddrinfo
           socket_create
           socket_freeaddrinfo
           socket_bind
           socket_listen
           socket_connect
           socket_accept
           socket_addrinfo_read

           ;; posix fd
           fd_read
           fd_write
           fd_close)
         (import (rnrs)
                 (yuni core)
                 (mosh ffi)
                 (srfi :8)
                 (nmosh ffi box)
                 (primitives bytevector-pointer pointer-copy!)
                 (prefix (nmosh stubs kqueue-stubs) stub:))

;; unix-fd thingy.. should move "posix-fd" or so

(define* fd (value))

(define (int->fd x)
  (unless (< 0 x)
    (assertion-violation 'int->fd "invalid argument" x))
  (make fd (value x)))

(define (fd? x)
  (is-a? x fd))

;; kevent

(define* kevent* (ptr n))

(define* (fd->int (fd))
  (let-with fd (value) value))

(define (kq_create) ;; => fd
  (int->fd (stub:kq_create)))

(define (kevent_alloc n)
  (let ((p (kevent_alloc n)))
    (make kevent*
          (ptr p)
          (n n))))

(define* (kevent_dispose (kevent*))
  (let-with kevent* (ptr)
    (stub:kevent_dispose ptr)))

(define* (kevent->pointer (kevent*) idx)
  (let-with kevent* (n ptr)
    (unless (< idx n)
      (assertion-violation 'kevent->pointer "invalid argument" n))
    (stub:kevent_offset ptr n)))

(define* (kevent_set_readevent! (kevent*) n (fd))
  (stub:kevent_set_readevent (kevent->pointer kevent* n)
                             (fd->int fd)))

(define* (kevent_set_writeevent! (kevent*) n (fd))
  (stub:kevent_set_writeevent (kevent->pointer kevent* n)
                             (fd->int fd)))

(define* (kevent_set_enableuserevent! (kevent*) n id)
  (stub:kevent_set_enableuserevent (kevent->pointer kevent* n)
                                   id))

(define* (kevent_set_triggeruserevent! (kevent*) n id)
  (stub:kevent_set_triggeruserevent (kevent->pointer kevent* n)
                                   id))

(define* (kevent_type (kevent*) n)
  (let ((i (stub:kevent_type (kevent->pointer kevent* n))))
    (case i
      ((0) 'USER)
      ((1) 'FILE)
      ((2) 'ERROR))))

(define* (kevent_ident (kevent*) n)
  (let ((p (kevent->pointer kevent* n)))
    (let ((x (stub:kevent_ident p))
          (t (kevent_type kevent* n)))
      (if (eq? t 'USER)
        x
        (int->fd x)))))

(define* (kevent_decode_fd (kevent*) n) ;; => read/write eof? data
  (let ((box-type (make-int-box))
        (box-eof (make-int-box))
        (box-data (make-int-box)))
    (stub:kevent_decode_fd (kevent->pointer kevent* n)
                           box-type
                           box-eof
                           box-data)
    (let ((type (int-box-ref box-type))
          (eof (int-box-ref box-eof))
          (data (int-box-ref box-data)))
      (values (if (= type 0) 'read 'write)
              (= eof 1) 
              data))))

(define null-pointer (integer->pointer 0))
(define (null-pointer? x) (= (pointer->integer x) 0))

(define* (kevent_exec (q fd) changes changeoffset changecount out outcount outoffset timeout)
  ;; FIXME: check bounds..
  (let ((changeptr (if changes (kevent->pointer changes changeoffset) null-pointer))
        (outptr (if out (kevent->pointer out outoffset) null-pointer)))
    (let ((ret (stub:kevent_exec (fd->int q)
                                 changecount
                                 changeptr
                                 outcount
                                 outptr
                                 timeout)))
      ret)))


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
        (service (if (number? service) (number->string service))))
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
    (let ((r (stub:socket_accept (int->fd fd)
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

(define* (fd_read (fd) buf len)
  (let ((r (stub:fd_read (fd->int fd)
                         buf
                         len)))
    r))
(define* (fd_write (fd) buf len)
  (let ((r (stub:fd_write (fd->int fd)
                          buf
                          len)))
    r))

(define* (fd_close (fd))
  (stub:fd_close (fd->int fd)))

)
