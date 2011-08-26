#include "config.h"
#ifdef HAVE_SIGACTION

#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#include "sigchld_handler.h"

/* NB:
 *  1. This handler is not thread safe at all.
 *  2. We should use signalfd() or kqueue of course. */

/* globals */
int sigchld_handler_fd;

static void
handler(int sig){
    unsigned int buf = 0;
    if(0<sigchld_handler_fd){
        write(sigchld_handler_fd,&buf,1);
    }
}

void
sigchld_handler_install(int fd){
    struct sigaction a;
    sigchld_handler_fd = fd;
    a.sa_handler = handler;
    sigaction(SIGCHLD,&a,NULL);
}

#endif
