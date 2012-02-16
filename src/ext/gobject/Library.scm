(mosh-gobj
  c-function-table
  *internal*
  (plugin: mosh_gobj)
  (libname: mosh-gobj)
  #(ret name args)
  (void* mglib_getwaitloop_func)
  (void* mglib_loop_new_waiter)
  (void mglib_init)
  (int mglib_loop_prepare)
  (void mglib_loop_acquire)
  (int mglib_fds_size)
  (int mglib_loop_start_wait (void* void* void* int))
  (int mglib_loop_dispatch (void* int))
  (int mglib_add_timeout (int fn void*))
  (void* mglib_timer_new)
  (void mglib_timer_start)
  (void mglib_timer_elapsed (void* void*)))
