(library (nmosh win32 aio)
         (export win32_iocp_create
                 win32_iocp_pop
                 win32_overlapped_alloc
                 win32_overlapped_free
                 win32_handle_read_async
                 win32_handle_write_async
                 win32_process_redirected_child2
                 win32_create_named_pipe_async
                 win32_wait_named_pipe_async)
         (import (rnrs)
                 (srfi :8)
                 (mosh ffi)
                 (yuni core)
                 (nmosh win32 util)
                 (prefix (nmosh stubs aio-win32) stub:))

(define* win32-handle (pointer))

;; FIXME: use sizeof ptr
(define (make-ptr-box) (make-bytevector 8))
(define (ptr-box-ref x)
  (integer->pointer (bytevector-u32-native-ref x 0)))

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

(define* (win32_handle_read_async (h win32-handle)
                                  offset
                                  len
                                  buf
                                  overlapped)
  (receive (hi lo) (split64 offset)
    (let ((x (stub:win32_handle_read_async (handle->pointer h)
                                           offset
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
                                           offset
                                           lo
                                           hi
                                           len
                                           buf
                                           overlapped)))
      x)))

(define (win32_process_redirected_child2 spec dir stdin stdout stderr)
  (define (pass x)
    (case x
      ((#t #f) (integer->pointer 0))
      (else
        (string->utf16-bv x))))
  (define (passf x) (if x 1 0))
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



)
