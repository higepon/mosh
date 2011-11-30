(library (nmosh aio impl win32 handle-ops)
         (export 
           ;; internal
           handle->stream

           ;; external
           queue-read0
           queue-write0
           queue-close0
           )
         (import (rnrs)
                 (yuni core)
                 (srfi :8)
                 (nmosh pffi interface)
                 (nmosh pffi win32 aio)
                 (nmosh aio impl win32 queue-iocp))

;; stream object for Win32 handle


;; Q callback == (^[bytes ovl key-obj] ...)

(define* win32-handle-stream (ovl/read ovl/write buf/read h))

(define BLKSIZE 4096)

(define* (handle->stream (Q) (win32-handle))
  (let ((stream (make win32-handle-stream
                      (ovl/read (win32_overlapped_alloc))
                      (ovl/write (win32_overlapped_alloc))
                      (h win32-handle))))
    (queue-register-handle Q win32-handle generic-callback stream)
    stream))

(define (generic-callback bytes ovl key)
  (let ((callback (pointer->object (win32_overlapped_getmydata ovl))))
    (assert (procedure? callback))
    (callback bytes)))

(define* (queue-read0 (Q) (win32-handle-stream) cb)
  ;; cb = (^[handle buf len] ...)
  (define buf (make-bytevector BLKSIZE))
  (define (check x)
    (when (= 0 x)
      (assert #f)))
  ;(display (list 'READ: cb))(newline)
  (let-with win32-handle-stream (ovl/read h)
    (define (callback bytes)
      (define next-buf (make-bytevector BLKSIZE))
      ;; callback
      (cb win32-handle-stream 
          (~ win32-handle-stream 'buf/read)
          (pointer->integer bytes))
      ;; Queue next read
      (~ win32-handle-stream 'buf/read := next-buf)
      (win32_overlapped_setmydata ovl/read (object->pointer callback))
      (check (win32_handle_read_async h
                                      0 ;; FIXME: Feed offset
                                      BLKSIZE
                                      next-buf
                                      ovl/read)))
    (win32_overlapped_setmydata ovl/read (object->pointer callback))
    (~ win32-handle-stream 'buf/read := buf)
    (check (win32_handle_read_async h
                                    0 ;; FIXME: Feed offset
                                    BLKSIZE
                                    buf
                                    ovl/read))))

(define* (queue-write0 (Q) (win32-handle-stream) data cb)
  ;; cb = (^[handle] ...)
  (define (check x)
    (when (= 0 x)
      (assert #f)))
  (define (callback bytes)
    (cb win32-handle-stream))
  (define buflen (bytevector-length data))
  (let-with win32-handle-stream (ovl/write h)
    (win32_overlapped_setmydata ovl/write (object->pointer callback))
    (check (win32_handle_write_async h
                                     0 ;; FIXME: Feed offset
                                     buflen
                                     data
                                     ovl/write))))

(define* (queue-close0 (Q) (win32-handle-stream))
  (let-with win32-handle-stream (ovl/read ovl/write h)
    (win32_overlapped_free ovl/read)
    (win32_overlapped_free ovl/write)
    (win32_handle_close h)))

)
