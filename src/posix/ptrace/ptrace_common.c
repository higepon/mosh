#include <sys/types.h>
#include <sys/ptrace.h>

#include "posix/ptrace/ptrace_common.h"

/* currently, FreeBSD only. */

int
call_ptrace(int req, uintptr_t pid, uintptr_t addr, int data){
    return ptrace(req,pid,(void *)addr,data);
}

int
ptrace_traceme(void){
    return call_ptrace(PT_TRACE_ME,0,0,0);
}
int
ptrace_read(uintptr_t pid, uintptr_t addr){
    return call_ptrace(PT_READ_D,pid,addr,0);
}

int
ptrace_write(uintptr_t pid, uintptr_t addr, int val){
    return call_ptrace(PT_WRITE_D,pid,addr,val);
}

int
ptrace_continue(uintptr_t pid, int sig){
    return call_ptrace(PT_CONTINUE, pid, 1, sig);
}

int
ptrace_singlestep(uintptr_t pid, int sig){
    return call_ptrace(PT_STEP, pid, 1, sig);
}

int
ptrace_attach(uintptr_t pid){
    return call_ptrace(PT_ATTACH, pid, 0, 0);
}

int
ptrace_detatch(uintptr_t pid){
    return call_ptrace(PT_DETACH, pid, 0, 0);
}

int
ptrace_regsize(void){
    return sizeof(struct reg);
}

int
ptrace_fpregsize(void){
    return sizeof(struct fpreg);
}

int
ptrace_getregs(uintptr_t pid, void* addr){
    return call_ptrace(PT_GETREGS, pid, (uintptr_t)addr, 0);
}

int
ptrace_setregs(uintptr_t pid, void* addr){
    return call_ptrace(PT_SETREGS, pid, (uintptr_t)addr, 0);
}

int
ptrace_getfpregs(uintptr_t pid, void* addr){
    return call_ptrace(PT_GETFPREGS, pid, (uintptr_t)addr, 0);
}

int
ptrace_setfpregs(uintptr_t pid, void* addr){
    return call_ptrace(PT_SETFPREGS, pid, (uintptr_t)addr, 0);
}
