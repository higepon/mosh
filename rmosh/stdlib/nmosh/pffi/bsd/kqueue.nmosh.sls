(library (nmosh pffi bsd kqueue)
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

           )
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (prefix (nmosh stubs kqueue-stubs) stub:))

;; kevent

(define* kevent* (ptr n))

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

)
