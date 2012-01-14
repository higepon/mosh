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

;; Q callback == (^[err bytes ovl key-obj] ...)

(define* win32-handle-stream (ovl/read ovl/write buf/read h))

(define BLKSIZE (* 128 1024))

(define* (handle->stream (Q) (win32-handle))
  (let ((stream (make win32-handle-stream
                      (ovl/read (win32_overlapped_alloc))
                      (ovl/write (win32_overlapped_alloc))
                      (h win32-handle))))
    (queue-register-handle Q win32-handle stream)
    stream))

(define* (queue-read0 (Q) (win32-handle-stream) cb)
  ;; cb = (^[handle buf len] ...)
  (define buf (make-bytevector BLKSIZE))

  ;(display (list 'READ: cb))(newline)
  (let-with win32-handle-stream (ovl/read h)
    (define (check queued-buf x)
      (cond 
        ((or (= -2 x) (= 0 x)) ;; 0 = EOF
         ;(display (list 'READ-FAIL0: (handle->pointer h)))(newline)
         (cb win32-handle-stream #f #f))
        ((= -1 x) 
         ;(display (list 'READ-QUEUED: (handle->pointer h)))(newline)
         'ok)
        (else ;; finished
          'ok
          ))
      #t)
    (define (callback err bytes ovl key)
      (define next-buf (make-bytevector BLKSIZE))
      ;; FIXME: trust bytes..?
      ;; callback

      ;(display (list 'read-callback err))(newline)
      (cond
        ((= err 0)
         ;(display (list 'READ-OK1: (handle->pointer h) (pointer->integer bytes)))(newline)
         (cb win32-handle-stream 
             (~ win32-handle-stream 'buf/read)
             (pointer->integer bytes)) 
         ;; Queue next read
         (~ win32-handle-stream 'buf/read := next-buf) 
         (win32_overlapped_setmydata ovl/read (object->pointer callback)) 
         (check next-buf
                (win32_handle_read_async h
                                         0 ;; FIXME: Feed offset
                                         BLKSIZE
                                         next-buf
                                         ovl/read)))
        ((= err 259) ;; ERROR_NO_MORE_ITEMS
         ;(display (list 'READ-ZERO1: (handle->pointer h)))(newline)
         ;; Queue next read
         (~ win32-handle-stream 'buf/read := next-buf) 
         (win32_overlapped_setmydata ovl/read (object->pointer callback)) 
         (check next-buf (win32_handle_read_async h
                                                  0
                                                  BLKSIZE
                                                  next-buf
                                                  ovl/read))
         'ok
         )
        (else
          ;(display (list 'READ-FAIL1: (handle->pointer h) err))(newline)
          (cb win32-handle-stream #f #f))))
    (win32_overlapped_setmydata ovl/read (object->pointer callback))
    (~ win32-handle-stream 'buf/read := buf)
    (check buf (win32_handle_read_async h
                                    0 ;; FIXME: Feed offset
                                    BLKSIZE
                                    buf
                                    ovl/read))))

(define* (queue-write0 (Q) (win32-handle-stream) data cb)
  ;; cb = (^[handle/#f] ...)
  (define buflen (bytevector-length data))
  (define (check x)
    (cond
      ((= -2 x) ;; Error
       ;(display (list 'WRITE-ERROR0:))(newline)
       (cb #f))
      ((= -1 x) ;; Queued
       ;(display (list 'WRITE-QUEUED0:))(newline)
       'ok)
      (else ;; Finished
        ;(display (list 'WRITE0: buflen))(newline)
        (unless (= x buflen)
          (assertion-violation 'queue-write0
                               "unmached length"
                               x 
                               buflen))
        ;(cb win32-handle-stream)
        )))

  (define (callback err bytes ovl key)
    (cond
      (bytes
        ;(display (list 'WRITE1: buflen))(newline)
        (cb win32-handle-stream))
      (else
        (cond
          ((= err 259) ;; Deferred(STATUS_PENDING)
           ;(display (list 'WRITE-RETRY1: err))(newline) 
           (cb win32-handle-stream)
           ;(queue-write0 Q win32-handle-stream data cb)
           )
          (else
            ;(display (list 'WRITE-ERROR1: err))(newline) 
            (cb #f)
            )))))

  (let-with win32-handle-stream (ovl/write h)
    ;(display (list 'write: buflen))(newline)
    (win32_overlapped_setmydata ovl/write (object->pointer callback))
    (check (win32_handle_write_async h
                                     0 ;; FIXME: Feed offset
                                     buflen
                                     data
                                     ovl/write))))

(define* (queue-close0 (Q) (win32-handle-stream))
  (let-with win32-handle-stream (ovl/read ovl/write h)
    (queue-unregister-handle Q h win32-handle-stream)
    ;; FIXME: Free it..
    ;(win32_overlapped_free ovl/read)
    ;(win32_overlapped_free ovl/write)
    (win32_handle_close h)))

)
