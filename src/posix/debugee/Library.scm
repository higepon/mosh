(posix-debugee
  c-function-table
  *internal*
  (libname: posix-debugee)
  (header: "posix/debugee/debugee.h")
  #(ret name args)
  ;; perform spawn
  (int debugee_spawn (int void* void* void* void*))
  ;; file actions
  (int debugee_fileactionssize)
  (void debugee_fileactions_init (void*))
  (void debugee_fileactions_destroy (void*))
  (void debugee_fileactions_adddup2 (void* int int))
  ;(void posixspawn_fileactions_addopen (void* void* int int))
  (void debugee_fileactions_addclose (void* int))
  )
