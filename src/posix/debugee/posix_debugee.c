#include "config.h"
#ifdef HAVE_VFORK

#ifndef HAVE_PTRACE
#define __WITHOUT_TRACE 1
#endif

#include "posix_debugee.h"

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef __WITHOUT_TRACE
/* ptrace stuff */
#include <sys/types.h>
#include <sys/ptrace.h>
#endif

typedef enum {
    ACT_DUP2,
    ACT_CLOSE 
} fileaction;

typedef struct {
    fileaction act;
    int param1;
    int param2;
} action;

typedef struct {
    int count;
    action act[1];
}actionptr;

static void
do_action(action a){
    switch(a.act){
        case ACT_DUP2:
            dup2(a.param1,a.param2);
            break;
        case ACT_CLOSE:
            close(a.param1);
            break;
    }
}


int
debugee_spawn(int trace_p,void* in_path,
        void* in_fileactions,void* in_argv,void* in_envp){
    int i;
    pid_t pid;
    actionptr* x = *(actionptr **)in_fileactions;
#ifdef __WITHOUT_TRACE
    if(trace_p){
        return -1;
    }
#endif

    /* perform vfork */
    pid = vfork();
    if(!pid){ /* child context */
        /* perform file actions */
        for(i=0;i!=x->count;i++){
            do_action(x->act[i]);
        }
        /* perform trace */
#ifndef __WITHOUT_TRACE
        if(trace_p){
            ptrace(PT_TRACE_ME,0,0,0);
        }
#endif
        /* perform exec */
        execve((const char*)in_path,in_argv,in_envp);
        exit(-1); /* FIXME: something to do..? */
    }else{
        return pid; /* -1 if error */
    }
}

int
debugee_fileactionssize(void){
    return sizeof(void*);
}

void
debugee_fileactions_init(void* p){
    actionptr* x;
    x = malloc(sizeof(actionptr));
    x->count = 0;
    *(actionptr **)p = x;
}

void
debugee_fileactions_destroy(void* p){
    actionptr* x = *(actionptr **)p;
    free(x);
}

static void
add1(void* p){
    actionptr* x = *(actionptr **)p;
    int size;
    size = sizeof(actionptr) + sizeof(action)*(x->count + 1);
    *(actionptr **)p = (actionptr *)realloc(x,size);
    x = *(actionptr **)p;
    x->count++;
}

void
debugee_fileactions_adddup2(void* p,int fd0,int fd1){
    actionptr* x;
    int o;
    add1(p);
    x = *(actionptr **)p;
    o = x->count-1;
    x->act[o].act = ACT_DUP2;
    x->act[o].param1 = fd0;
    x->act[o].param2 = fd1;
}

void
debugee_fileactions_addclose(void* p,int fd){
    actionptr* x;
    int o;
    add1(p);
    x = *(actionptr **)p;
    o = x->count-1;
    x->act[o].act = ACT_CLOSE;
    x->act[o].param1 = fd;
}

#endif
