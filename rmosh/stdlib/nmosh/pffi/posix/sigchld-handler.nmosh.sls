(library (nmosh pffi posix sigchld-handler)
         (export
           sigchld_handler_install)
         (import
           (rnrs)
           (yuni core)
           (nmosh pffi posix fd)
           (prefix (nmosh stubs posix-sigchld-handler) stub:))

(define initialized #f)
(define* (sigchld_handler_install (fd))
  (when initialized
    (assertion-violation 'sigchld_handler_install
                         "Attempted multiple sigchld_handler_install!"))
  (set! initialized #t)
  (stub:sigchld_handler_install (fd->int fd)))
)
