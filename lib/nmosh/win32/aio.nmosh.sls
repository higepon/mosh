(library (nmosh win32 aio)
         (export win32_iocp_create
                 win32_iocp_pop
                 win32_iocp_assoc
                 win32_overlapped_alloc
                 win32_overlapped_free
                 win32_overlapped_getmydata
                 win32_overlapped_setmydata
                 win32_handle_read_async
                 win32_handle_write_async
                 win32_process_redirected_child2
                 win32_create_named_pipe_async
                 win32_wait_named_pipe_async
                 win32_process_wait_async

                 ;; sockets
                 win32_sockaddr_storage_size
                 win32_socket_create
                 win32_getaddrinfo
                 win32_addrinfoex_free
                 win32_addrinfoex_read
                 win32_socket_connect
                 win32_socket_accept
                 win32_socket_bind
                 win32_socket_listen
                 )
         (import (rnrs)
                 (srfi :8)
                 (mosh ffi)
                 (yuni core)
                 (nmosh win32 util)
                 (prefix (nmosh stubs aio-win32) stub:))

(define null null-pointer)
(define (null-pointer? x)
  (and (pointer? x)
       (= 0 (pointer->integer x))))

(define* win32-handle (pointer))

;; FIXME: use sizeof ptr
(define (make-box-64) (make-bytevector 8))
(define (box-64-ref x)
  (bytevector-u64-native-ref x 0))
(define (box-64-ref-signed x)
  (bytevector-s64-native-ref x 0))

(define (make-box-32) (make-bytevector 4))
(define (box-32-ref x)
  (bytevector-u32-native-ref x 0))
(define (box-32-ref-signed x)
  (bytevector-s32-native-ref x 0))

;; We need run-time dispatch here. Because nmosh64 may execute nmosh32 cached 
;; code..
(define-syntax sel32/64
  (syntax-rules ()
    ((_ q p32 p64)
     (case q
       ((4) p32)
       ((8) p64)))))
(define make-ptr-box (sel32/64 size-of-void* make-box-32 make-box-64))
(define ptr-box-ref
  (let ((ref (sel32/64 size-of-void* box-32-ref box-64-ref)))
    (lambda (x) (integer->pointer (ref x)))))

(define make-int-box (sel32/64 size-of-int make-box-32 make-box-64))
(define int-box-ref (sel32/64 size-of-int box-32-ref-signed box-64-ref-signed))

(define (split64 x)
  (values 
    (bitwise-arithmetic-shift x -32)
    (bitwise-and x #xffffffff)))

(define (pointer->handle p)
  (unless (pointer? p)
    (assertion-violation 'make-handle "invalid argument" p))
  (make win32-handle
        (pointer p)))

(define* (handle->pointer (h win32-handle))
  (let-with h (pointer) pointer))

(define (win32_iocp_create)
  (let ((p (stub:win32_iocp_create)))
    (pointer->handle p)))

(define* (win32_iocp_assoc (iocp win32-handle) (h win32-handle) key)
  (let ((x (stub:win32_iocp_assoc (handle->pointer iocp)
                                  (handle->pointer h)
                                  key)))
    x))

(define* (win32_iocp_pop (iocp win32-handle) int)
  (let ((ret_bytestrans (make-ptr-box))
        (ret_key (make-ptr-box))
        (ret_overlapped (make-ptr-box)))
    (let ((x (stub:win32_iocp_pop (handle->pointer iocp)
                                  int
                                  ret_bytestrans
                                  ret_key
                                  ret_overlapped)))
      (values x 
              (ptr-box-ref ret_bytestrans)
              (ptr-box-ref ret_key)
              (ptr-box-ref ret_overlapped)))))

(define win32_overlapped_alloc stub:win32_overlapped_alloc)
(define win32_overlapped_free stub:win32_overlapped_free)
(define win32_overlapped_getmydata stub:win32_overlapped_getmydata)
(define win32_overlapped_setmydata stub:win32_overlapped_setmydata)

(define* (win32_handle_read_async (h win32-handle)
                                  offset
                                  len
                                  buf
                                  overlapped)
  (receive (hi lo) (split64 offset)
    (let ((x (stub:win32_handle_read_async (handle->pointer h)
                                           lo
                                           hi
                                           len
                                           buf
                                           overlapped)))
      x)))

(define* (win32_handle_write_async (h win32-handle)
                                  offset
                                  len
                                  buf
                                  overlapped)
  (receive (hi lo) (split64 offset)
    (let ((x (stub:win32_handle_write_async (handle->pointer h)
                                           lo
                                           hi
                                           len
                                           buf
                                           overlapped)))
      x)))

(define (win32_process_redirected_child2 spec dir stdin stdout stderr)
  (define (pass x)
    (case x
      ((#t #f) (integer->pointer 0)) ;; we no longer need this..
      (else
        (string->utf16-bv x))))
  (define (passf x)
    (if (string? x)
      (if (file-exists? x)
        3 ;; pass OPEN_EXISTING
        2) ;; pass CREATE_ALWAYS
      (if x 1 0)))
  (let ((x (stub:win32_process_redirected_child2
             (string->utf16-bv spec)
             (string->utf16-bv dir)
             (pass stdin)
             (pass stdout)
             (pass stderr)
             (passf stdin)
             (passf stdout)
             (passf stderr))))
    (pointer->handle x)))

(define (win32_create_named_pipe_async name)
  (let ((x (stub:win32_create_named_pipe_async (string->utf16-bv name))))
    (pointer->handle x)))


(define* (win32_wait_named_pipe_async (h win32-handle) overlapped)
  (let ((x (stub:win32_wait_named_pipe_async (handle->pointer h)
                                             overlapped)))
    x))

(define* (win32_process_wait_async (process win32-handle)
                                   (iocp win32-handle)
                                   key
                                   overlapped)
  (let ((x (stub:win32_process_wait_async (handle->pointer process)
                                          (handle->pointer iocp)
                                          key
                                          overlapped)))
    x))

;; socket
(define win32_sockaddr_storage_size stub:win32_sockaddr_storage_size)

;; mode = 0 .. UNSPEC, 4/6 = IP
;; proto = 1 .. TCP, 2 .. UDP
(define (win32_socket_create mode proto) ;; => (sock connectex acceptex)
                                         ;;    (sock #f #f) (UDP)
  (case proto
    ((1) ;; TCP
     (let ((ret_connectex (make-ptr-box))
           (ret_acceptex (make-ptr-box)))
       (let ((p (stub:win32_socket_create mode proto ret_connectex ret_acceptex)))
         (values (pointer->handle p)
                 (ptr-box-ref ret_connectex)
                 (ptr-box-ref ret_acceptex)))))
    ((2) ;; UDP
     (let ((p (stub:win32_socket_create mode proto null null)))
       (values (pointer->handle p)
               #f
               #f)))
    (else
      (assertion-violation 'win32_socket_create
                           "invalid protocol"
                           proto))))

(define* win32-addrinfoex (pointer))
(define (pointer->addrinfoex x)
  (if (pointer? x)
    (make win32-addrinfoex
          (pointer x))
    (assertion-violation 'pointer->addrinfoex
                         "invalid argument"
                         x)))
(define* (addrinfoex->pointer (x win32-addrinfoex))
  (let-with x (pointer) pointer))

(define (win32_getaddrinfo name servicename mode proto)
  (define (normalize-service x)
    (if (and (exact? x) (number? x))
      (number->string x)
      x))

  (let ((bv-name (string->utf16-bv name))
        (bv-serv (string->utf16-bv (normalize-service servicename)))
        (ret_addrinfoex (make-ptr-box)))
    (let ((r (stub:win32_getaddrinfo bv-name
                                     bv-serv
                                     ret_addrinfoex
                                     mode
                                     proto)))
      (pointer->addrinfoex (ptr-box-ref ret_addrinfoex)))))

(define* (win32_addrinfoex_free (x win32-addrinfoex))
  (stub:win32_addrinfoex_free (addrinfoex->pointer x)))

(define* (win32_addrinfoex_read (x win32-addrinfoex)) 
;; => (family sockaddr namelen next-addrinfo/#f)
  (let ((ret_family (make-ptr-box))
        (ret_sockaddr (make-ptr-box))
        (ret_namelen (make-int-box))
        (ret_next (make-ptr-box)))
    (stub:win32_addrinfoex_read (addrinfoex->pointer x)
                                ret_family
                                ret_sockaddr
                                ret_namelen
                                ret_next)
    (values (ptr-box-ref ret_family)
            (ptr-box-ref ret_sockaddr)
            (int-box-ref ret_namelen)
            (let ((p (ptr-box-ref ret_next)))
              (if (null-pointer? p)
                #f
                (pointer->addrinfoex p))))))


(define* (win32_socket_connect func (s win32-handle) addr namelen overlapped)
  (let ((s (stub:win32_socket_connect func
                                      (handle->pointer s)
                                      addr
                                      namelen
                                      overlapped)))
    s))

(define* (win32_socket_accept func 
                              (slisten win32-handle)
                              (saccept win32-handle)
                              buf
                              bufsize
                              overlapped)
  (let ((r (stub:win32_socket_accept func
                                     (handle->pointer slisten)
                                     (handle->pointer saccept)
                                     buf
                                     bufsize
                                     overlapped)))
    r))

(define* (win32_socket_bind (s win32-handle) name namelen)
  (stub:win32_socket_bind (handle->pointer s) name namelen))

(define* (win32_socket_listen/backlog-count (s win32-handle) l)
  (stub:win32_socket_listen (handle->pointer s) l))

(define win32_socket_listen
  (case-lambda
    ((s) (win32_socket_listen/backlog-count s 0))
    ((s l) (win32_socket_listen/backlog-count s l))))


)
