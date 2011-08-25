(posix-socket
  c-function-table
  *internal*
  (libname: posix-socket)
  (header: "posix_socket.h")
  #(ret name args)
  (int socket_sizeof_sockaddr_storage)
  (int socket_getaddrinfo (char* char* void* int int))
  (int socket_create (int int))
  (void socket_freeaddrinfo (void*))
  (int socket_bind (int void* int))
  (int socket_listen (int int))
  (int socket_connect (int void* int))
  (int socket_accept (int void* void*))
  (void socket_addrinfo_read (void* void* void* void* void*))
  (void socket_setnodelay (int)))
