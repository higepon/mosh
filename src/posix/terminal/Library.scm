(mosh-terminal
  c-function-table
  *internal*
  (libname: terminal)
  (header: "mosh_terminal.h")
  #(ret name args)
  (void terminal_acquire)
  (void terminal_release)
  (int  terminal_getsize)
  (int  terminal_isatty (int)))
  
