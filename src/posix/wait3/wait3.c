#include "config.h"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <stdio.h>

#include "wait3.h"

int
size_rusage(void){
    return sizeof(struct rusage);
}

int /* 0 = no process, -1 = fail , pid */
try_wait3(int* out_code,int* out_signal,void* out_ru){
    pid_t pid;
    int status;
    struct rusage* rg = (struct rusage*)out_ru;
    pid = wait3(&status, WNOHANG, rg);
    if(pid <= 0){
        return pid;
    }
    if(WIFEXITED(status)){
        *out_signal = 0;
        *out_code = WEXITSTATUS(status);
    }else{
        *out_signal = WTERMSIG(status);
        *out_code = -1;
    }
    return pid;
}
