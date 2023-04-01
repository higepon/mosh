;; generated from src/posix/wait3/Library.scm DO NOT EDIT!!
(library (nmosh stubs wait3)
(export try_wait3 size_rusage)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'wait3))


(define
  size_rusage
  (pffi-c-function %library int size_rusage))
(define
  try_wait3
  (pffi-c-function
    %library
    int
    try_wait3
    void*
    void*
    void*))
)
