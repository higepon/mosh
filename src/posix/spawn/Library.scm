(posixspawn
  c-function-table
  *internal*
  (libname: posixspawn)
  (header: "posix/spawn/posixspawn.h")
  #(ret name args)
  ;; perform spawn
  (int posixspawn_spawn (void* void* void* void* void*))
  ;; file actions
  (int posixspawn_fileactionssize)
  (void posixspawn_fileactions_init (void*))
  (void posixspawn_fileactions_destroy (void*))
  (void posixspawn_fileactions_adddup2 (void* int int))
  ;(void posixspawn_fileactions_addopen (void* void* int int))
  (void posixspawn_fileactions_addclose (void* int))
  )
