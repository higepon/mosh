(boehmgc-stubs
  c-function-table
  *internal*
  (libname: boehmgc-stubs)
  (header: "boehmgc-stubs.h")
  #(ret name args)
  (void* create_weak_vector (int))
  (void* weak_vector_ref (void* int))
  (void weak_vector_set (void* int void*))
  (void register_disappearing_link_wv (void* int void*))
  (void register_finalizer (void* void* void*))
  (void register_disappearing_link (void* void*))
  (void gcollect)
  (void genable_incremental)
  (int gcurrent_size)
  (int gfree_size)
  )
