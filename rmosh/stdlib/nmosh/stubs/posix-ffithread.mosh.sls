;; generated from src/posix/pthread/Library.scm DO NOT EDIT!!
(library (nmosh stubs posix-ffithread)
(export posix_invoke_ffithread)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'posix-ffithread))


(define
  posix_invoke_ffithread
  (pffi-c-function
    %library
    int
    posix_invoke_ffithread
    int
    void*
    void*
    void*))
)
