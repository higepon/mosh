#include "config.h"
#ifdef HAVE_FCNTL

#include "posix/fd/posix_fd.h"

#include <stdio.h>
/* BSD kqueue stubs */
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

/*** File Functions ... ***/
int
fd_read(int fd,void* buf,int len){
    int ret;
    ret = read(fd,buf,len);
    if(ret == -1){
        if(errno == EAGAIN){
            return 0;
        }else{
            return -1;
        }
    }else{
        return ret;
    }
}

int
fd_write(int fd,void* buf,int len){
    int ret;
    ret = write(fd,buf,len);
    if(ret == -1){
        if(errno == EAGAIN){
            return 0;
        }else{
            return -1;
        }
    }else{
        return ret;
    }
}

int
fd_close(int fd){
    return close(fd);
}

void
fd_setnonblock(int fd){
    int flg;
    flg = fcntl(fd,F_GETFL,0);
    fcntl(fd,F_SETFL,flg|O_NONBLOCK);
}

int
fd_pipe(int* in, int* out){
    int fds[2];
    int e;
    e = pipe(fds);
    *in = fds[0];
    *out = fds[1];
    return e;
}

#endif
