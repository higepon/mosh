;; generated from src/posix/fd/Library.scm DO NOT EDIT!!
(library (nmosh stubs posix-fd)
(export
  fd_setnonblock
  fd_pipe
  fd_close
  fd_write
  fd_read)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'posix-fd))


(define
  fd_read
  (pffi-c-function
    %library
    int
    fd_read
    int
    void*
    int))
(define
  fd_write
  (pffi-c-function
    %library
    int
    fd_write
    int
    void*
    int))
(define
  fd_close
  (pffi-c-function %library int fd_close int))
(define
  fd_pipe
  (pffi-c-function
    %library
    int
    fd_pipe
    void*
    void*))
(define
  fd_setnonblock
  (pffi-c-function
    %library
    void
    fd_setnonblock
    int))
)
