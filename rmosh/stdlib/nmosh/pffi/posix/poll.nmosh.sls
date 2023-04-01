(library (nmosh pffi posix poll)
         (export
           poll_get_fd
           poll_get_pollnval
           poll_get_pollhup
           poll_get_pollerr
           poll_get_pollout
           poll_get_pollin
           poll_unset_pollout
           poll_set_pollout
           poll_unset_pollin
           poll_set_pollin
           poll_set_fd
           poll_exec
           poll_dispose
           poll_alloc
           poll_copy!
           )
         (import (rnrs)
                 (yuni core)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (prefix (nmosh stubs posix-poll) stub:))

(define* pollfds (ptr fds pollin? pollout? n))

(define (poll_alloc n)
  (let ((p (stub:poll_alloc n)))
    (make pollfds 
      (ptr p)
      (fds (make-vector n))
      (pollin? (make-vector n #f))
      (pollout? (make-vector n #f))
      (n n))))

(define* (poll_dispose (pollfds))
  (let-with pollfds (ptr)
    (stub:poll_dispose ptr)))

(define* (poll_exec (pollfds) nfds timeout)
  (let-with pollfds (ptr n)
    ;; FIXME: check if nfds is valid
    (stub:poll_exec ptr nfds timeout)))

(define* (poll_set_fd (pollfds) pos fd)
  ;; fd: #f to clear fd
  (let-with pollfds (ptr n fds)
    (if fd
      (stub:poll_set_fd ptr pos (fd->int fd))
      (stub:poll_set_fd ptr pos -1))
    (vector-set! fds pos fd)))
(define* (poll_set_pollin (pollfds) pos)
  (let-with pollfds (ptr n pollin?)
    (vector-set! pollin? pos #t)
    (stub:poll_set_pollin ptr pos)))
(define* (poll_set_pollout (pollfds) pos)
  (let-with pollfds (ptr n pollout?)
    (vector-set! pollout? pos #t)
    (stub:poll_set_pollout ptr pos)))
(define* (poll_unset_pollin (pollfds) pos)
  (let-with pollfds (ptr n pollin?)
    (vector-set! pollin? pos #f)
    (stub:poll_unset_pollin ptr pos)))
(define* (poll_unset_pollout (pollfds) pos)
  (let-with pollfds (ptr n pollout?)
    (vector-set! pollout? pos #f)
    (stub:poll_unset_pollout ptr pos)))

(define-syntax def
  (syntax-rules ()
    ((_ name stubname)
     (define* (name (pollfds) pos)
       (let-with pollfds (ptr n)
         (let ((r (stubname ptr pos)))
           (if (= 1 r) #t #f)))))))

(def poll_get_pollin 
     stub:poll_get_pollin)

(def poll_get_pollout
     stub:poll_get_pollout)

(def poll_get_pollerr
     stub:poll_get_pollerr)

(def poll_get_pollhup
     stub:poll_get_pollhup)

(def poll_get_pollnval
     stub:poll_get_pollnval)

(define* (poll_get_fd (pollfds) pos)
  (let-with pollfds (fds)
    (vector-ref fds pos)))

(define* (poll_copy! (from pollfds) n-from (to pollfds) n-to)
  (let-with from (pollin? pollout? fds)
    (if (vector-ref pollin? n-from)
      (poll_set_pollin to n-to)
      (poll_unset_pollin to n-to))
    (if (vector-ref pollout? n-from)
      (poll_set_pollout to n-to)
      (poll_unset_pollout to n-to))
    (poll_set_fd to n-to (vector-ref fds n-from))
    (let-with to ((to-fds fds))
      (vector-set! to-fds n-to (vector-ref fds n-from)))))

)
