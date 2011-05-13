(win32-gui
  c-function-table
  *internal*
  (libname: win32-gui)
  (header: "aio_win32.h")
  #(ret name args)
  (int win32_messagebox (void* void* int int))
  (void win32_window_move (void* int int int int))
  (void win32_window_show (void* int))
  (void win32_window_hide (void*))
  (void win32_window_settitle (void* void*))
  (void win32_window_close (void*))
  (void win32_window_destroy (void*))
  (void win32_registerwindowclass)
  (void* win32_window_alloc)
  (void win32_window_fitbuffer (void* void*))
  (void win32_window_create (void* void*))
  (void win32_getmonitorinfo (int int void* void* void* void* void*))
  )
