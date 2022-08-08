#include "config.h"
#ifdef HAVE_KQUEUE

#include "bsd/kqueue/kqueue_stubs.h"

#include <stdio.h>
/* BSD kqueue stubs */
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/select.h>
#include <errno.h>
#include <unistd.h>

int
kq_create(void){
    return kqueue();
}

void*
kevent_alloc(int n){
    return malloc(sizeof(struct kevent)*n);
}

void*
kevent_offset(void* kep,int n){
    struct kevent* ke = (struct kevent *)kep;
    return &ke[n];
}

void
kevent_dispose(void* p){
    free(p);
}

void
kevent_set_readevent(void* ke,int fd){
    EV_SET((struct kevent*)ke,fd,EVFILT_READ,EV_ADD,0,0,NULL);
}

void
kevent_set_writeevent(void* ke,int fd){
    EV_SET((struct kevent*)ke,fd,EVFILT_WRITE,EV_ADD,0,0,NULL);
}

void
kevent_set_enableuserevent(void* ke,int id){
    EV_SET((struct kevent*)ke,id,EVFILT_USER,EV_ADD,0,0,NULL);
}

void
kevent_set_triggeruserevent(void* ke,int id){
    EV_SET((struct kevent*)ke,id,EVFILT_USER,0,NOTE_TRIGGER,0,NULL);
}

int
kevent_ident(void* kep){
    struct kevent* ke = (struct kevent *)kep;
    return (int)ke->ident;
}

/* 0:USER, 1:FILE(OTHERWISE), 2:ERROR */
int
kevent_type(void* kep){
    struct kevent* ke = (struct kevent *)kep;
    if(ke->flags & EV_ERROR){
        return 2;
    }
    switch(ke->filter){
        case EVFILT_USER:
            return 0;
    }
    return 1;
}

/* type = 0: READ, 1: WRITE */
void
kevent_decode_fd(void* kep,int* type,int* eofp,int* data){
    struct kevent* ke = (struct kevent *)kep;
    *type = (ke->filter == EVFILT_READ)?0:1;
    *eofp = (ke->flags & EV_EOF)?1:0;
    *data = ke->data;
}

#define MSEC (1000*1000)

int
kevent_exec(int q,int changecount, void* ke_changes,int count, void *ke_out,int timeout_ms){ /* -1 for infinite */
    int ret;
    struct timespec timeout;
    struct timespec* timeoutp;
    time_t sec;
    long nsec;

    if(timeout_ms == -1){
        timeoutp = NULL;
    }else{
        timeoutp = &timeout;
        sec = timeout_ms / 1000;
        nsec = (timeout_ms * MSEC) - (sec * MSEC);
        timeout.tv_sec = sec;
        timeout.tv_nsec = nsec;
    }

    ret = kevent(q,ke_changes,changecount,ke_out,count,timeoutp);

    return ret; /* -1 for error, but error can be queued and client should check for it */
}

#endif
