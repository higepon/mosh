(library (nmosh pffi win32 aio)
         (export win32_iocp_create
                 win32_iocp_pop
                 win32_iocp_assoc
                 win32_overlapped_alloc
                 win32_overlapped_free
                 win32_overlapped_getmydata
                 win32_overlapped_setmydata
                 win32_overlapped_geterror
                 win32_handle_read_async
                 win32_handle_write_async
                 win32_process_redirected_child2
                 win32_create_named_pipe_async
                 win32_wait_named_pipe_async
                 win32_process_wait_async
                 win32_handle_close

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

                 ;; GC related
                 win32_finalization_handler_alloc_overlapped
                 win32_register_finalization_event

                 ;; for gui
                 win32-handle
                 handle->pointer
                 )
         (import (rnrs)
                 (srfi :8)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi win32 util)
                 (nmosh finalizers)
                 (prefix (nmosh stubs aio-win32) stub:))

(define socket-handles (make-eq-hashtable))
(define* (add-socket! (h win32-handle))
  (let ((i (pointer->integer (handle->pointer h))))
    (hashtable-set! socket-handles i 'socket)))
(define* (remove-socket! (h win32-handle))
  (let ((i (pointer->integer (handle->pointer h))))
    (hashtable-delete! socket-handles i)))
(define* (socket? (h win32-handle))
  (let ((i (pointer->integer (handle->pointer h))))
    (eq? (hashtable-ref socket-handles i #f)
         'socket)))

(define null null-pointer)
(define (null-pointer? x)
  (and (pointer? x)
       (= 0 (pointer->integer x))))

(define* win32-handle (pointer))

(define (handle? x) (is-a? x win32-handle))

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

#|
(define (win32_process_pipe)
  (let ((box (make-ptr-array 2)))
    (define (ret x)
      (pointer->handle (ptr-array-ref box x)))
    (stub:win32_process_pipe box)
    (values (ret 0)
            (ret 1))))
|#

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
(define win32_overlapped_geterror stub:win32_overlapped_geterror)

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

(define pipehead "\\\\.\\pipe")
(define pipehead-len (string-length pipehead))
(define (named-pipe? x)
  (and (string? x)
       (string=? pipehead
                 (substring x 0 pipehead-len))))

(define (win32_process_redirected_child2 spec dir stdin stdout stderr)
  (define (pass x)
    (case x
      ((#t #f) (integer->pointer 0)) ;; we no longer need this..
      (else
        (cond
          ((string? x) (string->utf16-bv x))
          ((handle? x) (handle->pointer x))))))
  (define (pass-dir x)
    (cond
      ((string? x) (string->utf16-bv x))
      ((not x) (integer->pointer 0))))

  (define (passf x)
    (cond
      ((string? x)
       (if (or (named-pipe? x)  ;; Don't touch if x is named-pipe!
               (file-exists? x))
         3 ;; pass OPEN_EXISTING
         2)) ;; pass CREATE_ALWAYS
      ((handle? x)
       ;; pass 4 for handle
       4)
      ((not x) 0) ;; discard output
      ((eq? #t x) 1) ;; redirect to stdout/err
      (else (assertion-violation 'win32_process_redirected_child2
                                 "invalid argument"
                                 x))))
  (let ((x (stub:win32_process_redirected_child2
             (string->utf16-bv spec)
             (pass-dir dir)
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

(define* (win32_handle_close (h win32-handle))
  (let ((handle (handle->pointer h)))
    (if (socket? h)
      (let ((r (stub:win32_socket_close handle)))
        (remove-socket! h)
        r)
      (stub:win32_handle_close handle))))

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
       (let ((h (pointer->handle (stub:win32_socket_create mode proto ret_connectex ret_acceptex))))
         (add-socket! h)
         (values h
                 (ptr-box-ref ret_connectex)
                 (ptr-box-ref ret_acceptex)))))
    ((2) ;; UDP
     (let ((h (pointer->handle (stub:win32_socket_create mode proto null null))))
       (add-socket! h)
       (values h
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

(define* (win32_finalization_handler_alloc_overlapped (iocp win32-handle) key ptr)
  (stub:win32_finalization_handler_create (handle->pointer iocp)
                                          key
                                          ptr))

(define* (win32_register_finalization_event obj overlapped)
  (define finalizer (make-finalizer (stub:win32_finalization_handler_get)))
  (register-finalizer obj finalizer overlapped))

)
