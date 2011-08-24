(posix-fd
  c-function-table
  *internal*
  (libname: posix-fd)
  (header: "posix_fd.h")
  #(ret name args)
  (int fd_read (int void* int))
  (int fd_write (int void* int))
  (int fd_close (int))
  (int fd_pipe (void* void*))
  (void fd_setnonblock (int)))
