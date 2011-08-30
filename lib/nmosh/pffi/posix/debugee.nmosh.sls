(library (nmosh pffi posix debugee)
         (export
           debugee_spawn
           debugee_fileactionssize
           debugee_fileactions_init
           debugee_fileactions_destroy
           debugee_fileactions_adddup2
           debugee_fileactions_addclose)
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi posix util)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (prefix (nmosh stubs posix-debugee) stub:))

(define debugee_fileactionssize stub:debugee_fileactionssize)
(define debugee_fileactions_init stub:debugee_fileactions_init)
(define debugee_fileactions_destroy stub:debugee_fileactions_destroy)

(define* (debugee_fileactions_adddup2 bv (fd0 fd) (fd1 fd))
  (stub:debugee_fileactions_adddup2 bv 
                                    (fd->int fd0)
                                    (fd->int fd1)))

(define* (debugee_fileactions_addclose bv (fd))
  (stub:debugee_fileactions_addclose bv
                                     (fd->int fd)))

(define (debugee_spawn debug? path fileactions argv envp) ;; => pid/-1
  (let* ((d (if debug? 1 0))
         (r (stub:debugee_spawn d
                                (string->utf8/null path)
                                fileactions
                                (construct-string-ptrs argv)
                                (construct-string-ptrs envp))))
    r))

)
