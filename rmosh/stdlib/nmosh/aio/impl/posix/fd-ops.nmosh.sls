(library (nmosh aio impl posix fd-ops)
         (export
           queue-read0
           queue-write0
           queue-close0)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (nmosh pffi posix fd)
                 (nmosh aio impl posix queue-fd-poll))

(define BLKSIZE (* 128 1024))

(define (do-read Q fd callback)
  (let* ((buf (make-bytevector BLKSIZE))
         (len (fd_read fd buf BLKSIZE)))
    (if (<= len 0)
      (begin
        ;; something wrong
        (fd_close fd)
        (queue-unregister-fd Q fd)
        (callback fd #f #f))
      (callback fd buf len))))

(define (queue-read0 Q fd callback)
  (queue-register-fd/read Q fd
                          (^[fd event]
                            (case event
                              ((NVAL) ;; invalid fd
                               (queue-unregister-fd Q fd)
                               (callback fd #f #f))
                              ((HUP ERROR)
                               (fd_close fd)
                               (queue-unregister-fd Q fd)
                               (callback fd #f #f))
                              (else
                                (do-read Q fd callback))))))

(define (subbytevector bv off)
  (define len (- (bytevector-length bv) off))
  (define out (make-bytevector len))
  (bytevector-copy! bv off out 0 len)
  out)

(define (queue-write0 Q fd data callback)
  ;; Edge trigger
  (define len (bytevector-length data))
  (define off 0)
  (define (proc fd event)
    (let* ((out (subbytevector data off))
           (size (fd_write fd out (bytevector-length out))))
      ;(display (list 'q-write out))(newline)
      (cond
        ((<= 0 size)
         (set! off (+ off size))
         (when (= off len)
           (queue-unregister-fd/write Q fd)
           (callback fd)))
        (else
          (queue-unregister-fd/write Q fd)
          (callback #f)))))
  (cond
    ((= 0 len)
     (callback fd))
    (else
      (queue-register-fd/write Q fd proc))))

(define (queue-close0 Q fd)
  (queue-unregister-fd Q fd)
  (fd_close fd))

)
