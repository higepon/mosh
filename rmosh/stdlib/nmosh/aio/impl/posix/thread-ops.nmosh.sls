(library (nmosh aio impl posix thread-ops)
         (export queue-invoke-ffithread)
         (import (rnrs)
                 (srfi :8)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi posix ffithread)
                 (nmosh pffi posix fd)
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh aio impl posix fd-ops))

(define (invoke fd cb)
  (define out0 (make-bytevector size-of-pointer))
  (define out1 (make-bytevector size-of-pointer))
  (define (check c)
    (unless (= c size-of-pointer)
      (assertion-violation 'check
                           "short read"
                           c)))
  (check (fd_read fd out0 size-of-pointer))
  (check (fd_read fd out1 size-of-pointer))
  (cb (pointer->integer (ptr-box-ref out0)) (pointer->integer (ptr-box-ref out1))))

(define (queue-invoke-ffithread Q func in0 in1 cb)
  (define (callback fd evt)
    (case evt
      ((READ READ+WRITE) (invoke fd cb))
      (else
        (assertion-violation 'queue-invoke-ffithread
                             "Invalid event for queue-invoke-ffithread"
                             evt
                             (fd->int fd)))))
  (receive (in out) (fd_pipe)
    (ffithread-invoke (fd->int out) func (integer->pointer in0) (integer->pointer in1))
    (queue-register-fd/read Q in callback)))

)
