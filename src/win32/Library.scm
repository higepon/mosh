(aio-win32
  c-function-table
  *internal*
  (libname: aio-win32)
  (header: "aio_win32.h")
  #(ret name args)
  (void* win32_iocp_create)
  (int win32_iocp_assoc (void* void* void*))
  (int win32_iocp_pop (void* int void* void* void* void*))
  (void* win32_overlapped_alloc)
  (void win32_overlapped_free (void*))
  (int win32_handle_read_async (void* int int int void* void*))
  (int win32_handle_write_async (void* int int int void* void*))
  (void* win32_process_redirected_child2 (void* void* void* void* void* int int int))
  (void* win32_create_named_pipe_async (void*))
  (int win32_wait_named_pipe_async (void* void*)))
