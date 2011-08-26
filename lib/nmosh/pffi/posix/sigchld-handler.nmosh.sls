(library (nmosh pffi posix sigchld-handler)
         (export
           sigchld_handler_install)
         (import
           (rnrs)
           (yuni core)
           (nmosh pffi posix fd)
           (prefix (nmosh stubs posix-sigchld-handler) stub:))
(define* (sigchld_handler_install (fd))
  (stub:sigchld_handler_install (fd->int fd)))
)
