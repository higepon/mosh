;; generated from src/posix/environ/Library.scm DO NOT EDIT!!
(library (nmosh stubs environ)
(export environ_getptr)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'environ))


(define
  environ_getptr
  (pffi-c-function %library void* environ_getptr))
)
