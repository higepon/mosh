(library (nmosh io ext gobject0)
         (export 
           glib-init
           glib-start-mainloop
           glib-add-timeout
           glib-timer-new
           glib-timer-start
           glib-timer-elapsed
           )
         (import (rnrs)
                 (yuni core)
                 (shorten)
                 (match)
                 (only (mosh ffi) c-callback)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh io core)
                 (nmosh io master-queue)
                 (nmosh aio platform)
                 (nmosh stubs mosh-gobj))
;;

(define (glib-add-timeout msec cb)
  (mglib_add_timeout msec 
                     (c-callback int (void*) (^_ (let ((r (cb))) (if r 1 0))))
                     (integer->pointer 0)))

(define (glib-timer-new) (mglib_timer_new))
(define (glib-timer-start t) (mglib_timer_start t))
(define (glib-timer-elapsed t) ;; => nanosec
  (let ((b (make-ptr-box)))
    (mglib_timer_elapsed t b)
    (ptr-box-ref-unsigned b)))

(define (glib-init) (mglib_init))
(define (glib-start-mainloop)
  (define nfds 1024)
  (define fds (make-bytevector (* (mglib_fds_size) nfds)))
  (define c (make-ptr-array 3))
  (define waitloop (mglib_getwaitloop_func))
  (define q (mglib_loop_new_waiter))
  (define count 0)
  (define (callback out0 out1)
    (mglib_loop_dispatch fds count)
    (kick))
  (define (kick)
    (mglib_loop_prepare)
    (let ((fcount (mglib_loop_start_wait q c fds nfds)))
      (set! count fcount)))
  (mglib_loop_acquire)
  (queue-invoke-ffithread nmosh-io-master-queue
                          waitloop
                          (pointer->integer q)
                          nfds
                          callback)

  (kick))

)
