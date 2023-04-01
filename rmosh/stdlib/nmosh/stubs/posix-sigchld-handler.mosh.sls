;; generated from src/posix/sigchld_handler/Library.scm DO NOT EDIT!!
(library (nmosh stubs posix-sigchld-handler)
(export sigchld_handler_install)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'posix-sigchld-handler))


(define
  sigchld_handler_install
  (pffi-c-function
    %library
    void
    sigchld_handler_install
    int))
)
