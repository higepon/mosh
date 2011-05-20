(ptrace-common
  c-function-table
  *internal*
  (libname: ptrace-common)
  (header: "posix/ptrace/ptrace_common.h")
  #(ret name args)
  (int call_ptrace (int int int int))
  (int ptrace_traceme)
  (int ptrace_read (int int))
  (int ptrace_write (int int int))
  (int ptrace_continue (int int))
  (int ptrace_singlestep (int int))
  (int ptrace_attach (int))
  (int ptrace_detatch (int))
  (int ptrace_regsize)
  (int ptrace_getregs (int void*))
  (int ptrace_setregs (int void*))
  (int ptrace_fpregsize)
  (int ptrace_getfpregs (int void*))
  (int ptrace_setfpregs (int void*)))
  
