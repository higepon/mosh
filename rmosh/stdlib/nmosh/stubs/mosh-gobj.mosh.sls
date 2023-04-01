;; generated from src/ext/gobject/Library.scm DO NOT EDIT!!
(library (nmosh stubs mosh-gobj)
(export
  mglib_timer_elapsed
  mglib_timer_start
  mglib_timer_new
  mglib_add_timeout
  mglib_loop_dispatch
  mglib_loop_start_wait
  mglib_fds_size
  mglib_loop_acquire
  mglib_loop_prepare
  mglib_init
  mglib_loop_new_waiter
  mglib_getwaitloop_func)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi-plugin)
  (nmosh ffi stublib))


(define %library (make-pffi-ref/plugin 'mosh_gobj))


(define
  mglib_getwaitloop_func
  (pffi-c-function
    %library
    void*
    mglib_getwaitloop_func))
(define
  mglib_loop_new_waiter
  (pffi-c-function
    %library
    void*
    mglib_loop_new_waiter))
(define
  mglib_init
  (pffi-c-function %library void mglib_init))
(define
  mglib_loop_prepare
  (pffi-c-function %library int mglib_loop_prepare))
(define
  mglib_loop_acquire
  (pffi-c-function
    %library
    void
    mglib_loop_acquire))
(define
  mglib_fds_size
  (pffi-c-function %library int mglib_fds_size))
(define
  mglib_loop_start_wait
  (pffi-c-function
    %library
    int
    mglib_loop_start_wait
    void*
    void*
    void*
    int))
(define
  mglib_loop_dispatch
  (pffi-c-function
    %library
    int
    mglib_loop_dispatch
    void*
    int))
(define
  mglib_add_timeout
  (pffi-c-function
    %library
    int
    mglib_add_timeout
    int
    callback
    void*))
(define
  mglib_timer_new
  (pffi-c-function %library void* mglib_timer_new))
(define
  mglib_timer_start
  (pffi-c-function %library void mglib_timer_start))
(define
  mglib_timer_elapsed
  (pffi-c-function
    %library
    void
    mglib_timer_elapsed
    void*
    void*))
)
