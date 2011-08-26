#include "config.h"
#ifdef HAVE_POLL

#include "posix/poll/posix_poll.h"

#include <stdlib.h>
#include <poll.h>

void*
poll_alloc(int count){
    struct pollfd* p;
    int i;
    p = (struct pollfd *)malloc(sizeof(struct pollfd) * count);
    for(i=0;i!=count;i++){
        p[i].fd = -1;
    }
    return p;
}

void
poll_dispose(void* p){
    free(p);
}

int
poll_exec(void* p,int count,int timeout){
    struct pollfd* pfd = (struct pollfd *)p;
    return poll(pfd,count,timeout);
}

void
poll_set_fd(void* p,int pos,int fd){
    struct pollfd* pfd = (struct pollfd *)p;
    pfd[pos].fd = fd;
}

void
poll_set_pollin(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    pfd[pos].events |= POLLIN;
}

void
poll_unset_pollin(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    pfd[pos].events &= ~POLLIN;
}

void
poll_set_pollout(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    pfd[pos].events |= POLLOUT;
}

void
poll_unset_pollout(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    pfd[pos].events &= ~POLLOUT;
}

int
poll_get_pollin(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    return (pfd[pos].revents & POLLIN) ? 1:0;
}

int
poll_get_pollout(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    return (pfd[pos].revents & POLLOUT) ? 1:0;
}

int
poll_get_pollerr(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    return (pfd[pos].revents & POLLERR) ? 1:0;
}
int
poll_get_pollhup(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    return (pfd[pos].revents & POLLHUP) ? 1:0;
}
int
poll_get_pollnval(void* p,int pos){
    struct pollfd* pfd = (struct pollfd *)p;
    return (pfd[pos].revents & POLLNVAL) ? 1:0;
}

#endif
