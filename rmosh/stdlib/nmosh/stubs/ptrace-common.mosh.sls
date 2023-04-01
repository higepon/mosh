;; generated from src/posix/ptrace/Library.scm DO NOT EDIT!!
(library (nmosh stubs ptrace-common)
(export
  ptrace_setfpregs
  ptrace_getfpregs
  ptrace_fpregsize
  ptrace_setregs
  ptrace_getregs
  ptrace_regsize
  ptrace_detatch
  ptrace_attach
  ptrace_singlestep
  ptrace_continue
  ptrace_write
  ptrace_read
  ptrace_traceme
  call_ptrace)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define %library (make-pffi-ref 'ptrace-common))


(define
  call_ptrace
  (pffi-c-function
    %library
    int
    call_ptrace
    int
    int
    int
    int))
(define
  ptrace_traceme
  (pffi-c-function %library int ptrace_traceme))
(define
  ptrace_read
  (pffi-c-function
    %library
    int
    ptrace_read
    int
    int))
(define
  ptrace_write
  (pffi-c-function
    %library
    int
    ptrace_write
    int
    int
    int))
(define
  ptrace_continue
  (pffi-c-function
    %library
    int
    ptrace_continue
    int
    int))
(define
  ptrace_singlestep
  (pffi-c-function
    %library
    int
    ptrace_singlestep
    int
    int))
(define
  ptrace_attach
  (pffi-c-function %library int ptrace_attach int))
(define
  ptrace_detatch
  (pffi-c-function %library int ptrace_detatch int))
(define
  ptrace_regsize
  (pffi-c-function %library int ptrace_regsize))
(define
  ptrace_getregs
  (pffi-c-function
    %library
    int
    ptrace_getregs
    int
    void*))
(define
  ptrace_setregs
  (pffi-c-function
    %library
    int
    ptrace_setregs
    int
    void*))
(define
  ptrace_fpregsize
  (pffi-c-function %library int ptrace_fpregsize))
(define
  ptrace_getfpregs
  (pffi-c-function
    %library
    int
    ptrace_getfpregs
    int
    void*))
(define
  ptrace_setfpregs
  (pffi-c-function
    %library
    int
    ptrace_setfpregs
    int
    void*))
)
