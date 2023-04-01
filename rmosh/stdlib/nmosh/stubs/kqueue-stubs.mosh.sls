;; generated from src/bsd/kqueue/Library.scm DO NOT EDIT!!
(library (nmosh stubs kqueue-stubs)
(export
  kevent_exec
  kevent_decode_fd
  kevent_type
  kevent_ident
  kevent_set_triggeruserevent
  kevent_set_enableuserevent
  kevent_set_writeevent
  kevent_set_readevent
  kevent_dispose
  kevent_offset
  kevent_alloc
  kq_create)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'kqueue-stubs))


(define
  kq_create
  (pffi-c-function %library int kq_create))
(define
  kevent_alloc
  (pffi-c-function %library void* kevent_alloc int))
(define
  kevent_offset
  (pffi-c-function
    %library
    void*
    kevent_offset
    void*
    int))
(define
  kevent_dispose
  (pffi-c-function
    %library
    void
    kevent_dispose
    void*))
(define
  kevent_set_readevent
  (pffi-c-function
    %library
    void
    kevent_set_readevent
    void*
    int))
(define
  kevent_set_writeevent
  (pffi-c-function
    %library
    void
    kevent_set_writeevent
    void*
    int))
(define
  kevent_set_enableuserevent
  (pffi-c-function
    %library
    void
    kevent_set_enableuserevent
    void*
    int))
(define
  kevent_set_triggeruserevent
  (pffi-c-function
    %library
    void
    kevent_set_triggeruserevent
    void*
    int))
(define
  kevent_ident
  (pffi-c-function %library int kevent_ident void*))
(define
  kevent_type
  (pffi-c-function %library int kevent_type void*))
(define
  kevent_decode_fd
  (pffi-c-function
    %library
    void
    kevent_decode_fd
    void*
    void*
    void*
    void*))
(define
  kevent_exec
  (pffi-c-function
    %library
    int
    kevent_exec
    int
    int
    void*
    int
    void*
    int))
)
