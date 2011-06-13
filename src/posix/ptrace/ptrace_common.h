#ifdef __cplusplus
extern "C"{
#endif
int call_ptrace(int req, uintptr_t pid, uintptr_t addr, int data);
int ptrace_traceme(void);
int ptrace_read(uintptr_t pid, uintptr_t addr);
int ptrace_write(uintptr_t pid,uintptr_t addr,int val);
int ptrace_continue(uintptr_t pid,int sig);
int ptrace_singlestep(uintptr_t pid, int sig);
int ptrace_attach(uintptr_t pid);
int ptrace_detatch(uintptr_t pid);
int ptrace_regsize(void);
int ptrace_fpregsize(void);
int ptrace_getregs(uintptr_t pid,void* addr);
int ptrace_setregs(uintptr_t pid,void* addr);
int ptrace_getfpregs(uintptr_t pid,void* addr);
int ptrace_setfpregs(uintptr_t pid,void* addr);
#ifdef __cplusplus
};
#endif


