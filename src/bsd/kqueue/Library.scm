(kqueue-stubs
  c-function-table
  *internal*
  (libname: kqueue-stubs)
  (header: "kqueue_stubs.h")
  #(ret name args)
  (int kq_create)
  (void* kevent_alloc (int))
  (void* kevent_offset (void* int))
  (void kevent_dispose (void*))
  (void kevent_set_readevent (void* int))
  (void kevent_set_writeevent (void* int))
  (void kevent_set_enableuserevent (void* int))
  (void kevent_set_triggeruserevent (void* int))
  (int kevent_ident (void*))
  (int kevent_type (void*))
  (void kevent_decode_fd (void* void* void* void*))
  (int kevent_exec (int int void* int void* int)))
