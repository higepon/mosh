(library (nmosh pffi posix fd)
         (export 
           ;; posix fd
           fd_read
           fd_write
           fd_close
           fd_setnonblock
           fd_pipe
           
           fd->int
           int->fd

           fd
           )
           
         (import (rnrs)
                 (yuni core)
                 (nmosh pffi interface)
                 (srfi :8)
                 (nmosh ffi box)
                 (prefix (nmosh stubs posix-fd) stub:))

;; unix-fd thingy..

(define* fd (value))

(define (int->fd x)
  (unless (< 0 x)
    (assertion-violation 'int->fd "invalid argument" x))
  (make fd (value x)))

(define (fd? x)
  (is-a? x fd))

(define* (fd->int (fd))
  (let-with fd (value) value))

(define null-pointer (integer->pointer 0))
(define (null-pointer? x) (= (pointer->integer x) 0))


(define* (fd_read (fd) buf len)
  (let ((r (stub:fd_read (fd->int fd)
                         buf
                         len)))
    r))
(define* (fd_write (fd) buf len)
  (let ((r (stub:fd_write (fd->int fd)
                          buf
                          len)))
    r))

(define* (fd_close (fd))
  (stub:fd_close (fd->int fd)))

(define* (fd_setnonblock (fd))
  (stub:fd_setnonblock (fd->int fd)))

(define (fd_pipe)
  (let ((in (make-int-box))
        (out (make-int-box)))
    (stub:fd_pipe in out)
    (values (int->fd (int-box-ref in))
            (int->fd (int-box-ref out)))))

)
