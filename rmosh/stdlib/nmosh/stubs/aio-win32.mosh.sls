;; generated from src/win32/aio/Library.scm DO NOT EDIT!!
(library (nmosh stubs aio-win32)
(export
  win32_finalization_handler_create
  win32_finalization_handler_get
  win32_socket_getsockname
  win32_socket_listen
  win32_socket_bind
  win32_socket_accept
  win32_socket_connect
  win32_socket_close
  win32_addrinfoex_read
  win32_addrinfoex_free
  win32_getaddrinfo
  win32_socket_create
  win32_sockaddr_storage_size
  win32_process_wait_async
  win32_wait_named_pipe_async
  win32_create_named_pipe_async
  win32_process_redirected_child2
  win32_handle_write_async
  win32_handle_read_async
  win32_handle_close
  win32_overlapped_geterror
  win32_overlapped_setmydata
  win32_overlapped_getmydata
  win32_overlapped_free
  win32_overlapped_alloc
  win32_iocp_pop
  win32_iocp_assoc
  win32_iocp_create)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'aio-win32))


(define
  win32_iocp_create
  (pffi-c-function
    %library
    void*
    win32_iocp_create))
(define
  win32_iocp_assoc
  (pffi-c-function
    %library
    int
    win32_iocp_assoc
    void*
    void*
    void*))
(define
  win32_iocp_pop
  (pffi-c-function
    %library
    int
    win32_iocp_pop
    void*
    int
    void*
    void*
    void*))
(define
  win32_overlapped_alloc
  (pffi-c-function
    %library
    void*
    win32_overlapped_alloc))
(define
  win32_overlapped_free
  (pffi-c-function
    %library
    void
    win32_overlapped_free
    void*))
(define
  win32_overlapped_getmydata
  (pffi-c-function
    %library
    void*
    win32_overlapped_getmydata
    void*))
(define
  win32_overlapped_setmydata
  (pffi-c-function
    %library
    void
    win32_overlapped_setmydata
    void*
    void*))
(define
  win32_overlapped_geterror
  (pffi-c-function
    %library
    int
    win32_overlapped_geterror
    void*))
(define
  win32_handle_close
  (pffi-c-function
    %library
    int
    win32_handle_close
    void*))
(define
  win32_handle_read_async
  (pffi-c-function
    %library
    int
    win32_handle_read_async
    void*
    int
    int
    int
    void*
    void*))
(define
  win32_handle_write_async
  (pffi-c-function
    %library
    int
    win32_handle_write_async
    void*
    int
    int
    int
    void*
    void*))
(define
  win32_process_redirected_child2
  (pffi-c-function
    %library
    void*
    win32_process_redirected_child2
    void*
    void*
    void*
    void*
    void*
    int
    int
    int))
(define
  win32_create_named_pipe_async
  (pffi-c-function
    %library
    void*
    win32_create_named_pipe_async
    void*))
(define
  win32_wait_named_pipe_async
  (pffi-c-function
    %library
    int
    win32_wait_named_pipe_async
    void*
    void*))
(define
  win32_process_wait_async
  (pffi-c-function
    %library
    int
    win32_process_wait_async
    void*
    void*
    void*
    void*))
(define
  win32_sockaddr_storage_size
  (pffi-c-function
    %library
    int
    win32_sockaddr_storage_size))
(define
  win32_socket_create
  (pffi-c-function
    %library
    void*
    win32_socket_create
    int
    int
    void*
    void*))
(define
  win32_getaddrinfo
  (pffi-c-function
    %library
    int
    win32_getaddrinfo
    void*
    void*
    void*
    int
    int))
(define
  win32_addrinfoex_free
  (pffi-c-function
    %library
    void
    win32_addrinfoex_free
    void*))
(define
  win32_addrinfoex_read
  (pffi-c-function
    %library
    void
    win32_addrinfoex_read
    void*
    void*
    void*
    void*
    void*))
(define
  win32_socket_close
  (pffi-c-function
    %library
    int
    win32_socket_close
    void*))
(define
  win32_socket_connect
  (pffi-c-function
    %library
    int
    win32_socket_connect
    void*
    void*
    void*
    int
    void*))
(define
  win32_socket_accept
  (pffi-c-function
    %library
    int
    win32_socket_accept
    void*
    void*
    void*
    void*
    int
    void*))
(define
  win32_socket_bind
  (pffi-c-function
    %library
    int
    win32_socket_bind
    void*
    void*
    int))
(define
  win32_socket_listen
  (pffi-c-function
    %library
    int
    win32_socket_listen
    void*
    int))
(define
  win32_socket_getsockname
  (pffi-c-function
    %library
    int
    win32_socket_getsockname
    void*
    void*
    int))
(define
  win32_finalization_handler_get
  (pffi-c-function
    %library
    void*
    win32_finalization_handler_get))
(define
  win32_finalization_handler_create
  (pffi-c-function
    %library
    void*
    win32_finalization_handler_create
    void*
    void*
    void*))
)