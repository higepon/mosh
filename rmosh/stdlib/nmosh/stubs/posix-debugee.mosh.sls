;; generated from src/posix/debugee/Library.scm DO NOT EDIT!!
(library (nmosh stubs posix-debugee)
(export
  debugee_fileactions_addclose
  debugee_fileactions_adddup2
  debugee_fileactions_destroy
  debugee_fileactions_init
  debugee_fileactionssize
  debugee_spawn)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'posix-debugee))


(define
  debugee_spawn
  (pffi-c-function
    %library
    int
    debugee_spawn
    int
    void*
    void*
    void*
    void*))
(define
  debugee_fileactionssize
  (pffi-c-function
    %library
    int
    debugee_fileactionssize))
(define
  debugee_fileactions_init
  (pffi-c-function
    %library
    void
    debugee_fileactions_init
    void*))
(define
  debugee_fileactions_destroy
  (pffi-c-function
    %library
    void
    debugee_fileactions_destroy
    void*))
(define
  debugee_fileactions_adddup2
  (pffi-c-function
    %library
    void
    debugee_fileactions_adddup2
    void*
    int
    int))
(define
  debugee_fileactions_addclose
  (pffi-c-function
    %library
    void
    debugee_fileactions_addclose
    void*
    int))
)
