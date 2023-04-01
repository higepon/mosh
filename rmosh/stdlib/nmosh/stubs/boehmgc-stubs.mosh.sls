;; generated from src/generic/Library.scm DO NOT EDIT!!
(library (nmosh stubs boehmgc-stubs)
(export
  gset_time_limit
  gfree_size
  gcurrent_size
  genable_incremental
  gcollect
  register_disappearing_link
  register_finalizer
  register_disappearing_link_wv
  weak_vector_set
  weak_vector_ref
  create_weak_vector)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'boehmgc-stubs))


(define
  create_weak_vector
  (pffi-c-function
    %library
    void*
    create_weak_vector
    int))
(define
  weak_vector_ref
  (pffi-c-function
    %library
    void*
    weak_vector_ref
    void*
    int))
(define
  weak_vector_set
  (pffi-c-function
    %library
    void
    weak_vector_set
    void*
    int
    void*))
(define
  register_disappearing_link_wv
  (pffi-c-function
    %library
    void
    register_disappearing_link_wv
    void*
    int
    void*))
(define
  register_finalizer
  (pffi-c-function
    %library
    void
    register_finalizer
    void*
    void*
    void*))
(define
  register_disappearing_link
  (pffi-c-function
    %library
    void
    register_disappearing_link
    void*
    void*))
(define
  gcollect
  (pffi-c-function %library void gcollect))
(define
  genable_incremental
  (pffi-c-function
    %library
    void
    genable_incremental))
(define
  gcurrent_size
  (pffi-c-function %library int gcurrent_size))
(define
  gfree_size
  (pffi-c-function %library int gfree_size))
(define
  gset_time_limit
  (pffi-c-function
    %library
    void
    gset_time_limit
    int))
)
