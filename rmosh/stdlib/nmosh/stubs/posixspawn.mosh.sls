;; generated from src/posix/spawn/Library.scm DO NOT EDIT!!
(library (nmosh stubs posixspawn)
(export
  posixspawn_fileactions_addclose
  posixspawn_fileactions_adddup2
  posixspawn_fileactions_destroy
  posixspawn_fileactions_init
  posixspawn_fileactionssize
  posixspawn_spawn)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'posixspawn))


(define
  posixspawn_spawn
  (pffi-c-function
    %library
    int
    posixspawn_spawn
    void*
    void*
    void*
    void*
    void*))
(define
  posixspawn_fileactionssize
  (pffi-c-function
    %library
    int
    posixspawn_fileactionssize))
(define
  posixspawn_fileactions_init
  (pffi-c-function
    %library
    void
    posixspawn_fileactions_init
    void*))
(define
  posixspawn_fileactions_destroy
  (pffi-c-function
    %library
    void
    posixspawn_fileactions_destroy
    void*))
(define
  posixspawn_fileactions_adddup2
  (pffi-c-function
    %library
    void
    posixspawn_fileactions_adddup2
    void*
    int
    int))
(define
  posixspawn_fileactions_addclose
  (pffi-c-function
    %library
    void
    posixspawn_fileactions_addclose
    void*
    int))
)
