(posix-poll
  c-function-table
  *internal*
  (libname: posix-poll)
  (header: "posix_poll.h")
  #(ret name args)
  (void* poll_alloc (int))
  (void poll_dispose (void*))
  (int poll_exec (void* int int))
  (void poll_set_fd (void* int int))
  (void poll_set_pollin (void* int))
  (void poll_unset_pollin (void* int))
  (void poll_set_pollout (void* int))
  (void poll_unset_pollout (void* int))
  (int poll_get_pollin (void* int))
  (int poll_get_pollout (void* int))
  (int poll_get_pollerr (void* int))
  (int poll_get_pollhup (void* int))
  (int poll_get_pollnval (void* int)))
