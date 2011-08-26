(library (nmosh pffi posix spawn)
         (export
           posixspawn_spawn
           posixspawn_fileactionssize
           posixspawn_fileactions_init
           posixspawn_fileactions_destroy
           posixspawn_fileactions_adddup2
           posixspawn_fileactions_addclose)
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi posix util)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (prefix (nmosh stubs posixspawn) stub:))

(define posixspawn_fileactionssize stub:posixspawn_fileactionssize)
(define posixspawn_fileactions_init stub:posixspawn_fileactions_init)
(define posixspawn_fileactions_destroy stub:posixspawn_fileactions_destroy)

(define* (posixspawn_fileactions_adddup2 bv (fd0 fd) (fd1 fd))
  (stub:posixspawn_fileactions_adddup2 bv 
                                       (fd->int fd0)
                                       (fd->int fd1)))

(define* (posixspawn_fileactions_addclose bv (fd))
  (stub:posixspawn_fileactions_addclose bv
                                        (fd->int fd)))

(define (posixspawn_spawn path fileactions argv envp) ;; => pid/errno
  (let* ((out-pid (make-int-box))
         (r (stub:posixspawn_spawn out-pid
                                   (string->utf8 path)
                                   fileactions
                                   (construct-string-ptrs argv)
                                   (construct-string-ptrs envp))))
    (if (= r 0)
      (int-box-ref out-pid)
      r)))

)
