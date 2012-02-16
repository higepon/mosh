#include <glib.h>
#include "config.h"
#include <stdio.h>

static uintptr_t
waitloop(uintptr_t in0, uintptr_t in1, uintptr_t* out0,uintptr_t* out1){
    GPollFD* fds;
    int nfds;
	int timeout;
    void** c;
    int r;
    // out0: status
    // out1: ZERO
    c = (void **)g_async_queue_pop((GAsyncQueue *)in0);
    fds = (GPollFD*)c[0];
    nfds = (uintptr_t)(void*)c[1];
	timeout = (intptr_t)(void*)c[2];
	//printf("pop %x count %d timeout %d\n",(int)fds,nfds,timeout);
    r = g_poll(fds, nfds, timeout);
	//printf("poll ret = %d\n",r);
    return 1;
}

MOSHEXPORT
void*
mglib_getwaitloop_func(void){
    return &waitloop;
}

MOSHEXPORT
void*
mglib_loop_new_waiter(void){
    return g_async_queue_new();
}

MOSHEXPORT
void
mglib_init(void){
    g_thread_init(NULL);
}

MOSHEXPORT
int
mglib_loop_prepare(void){
    gboolean b;
    gint bogus;
    b = g_main_context_prepare(g_main_context_default(), &bogus);
    return b ? 1 : 0;
}

MOSHEXPORT
int
mglib_fds_size(void){
    return sizeof(GPollFD);
}

MOSHEXPORT
void
mglib_loop_acquire(void){
    g_main_context_acquire(g_main_context_default());
}

MOSHEXPORT
int
mglib_loop_start_wait(void* q, void** c, void* fds, int len){
    int count;
    gint timeout;
    count = g_main_context_query(g_main_context_default(),
                                 999999, /* FIXME: */
                                 &timeout,
                                 fds,
                                 len);
    c[0] = fds;
    c[1] = (uintptr_t)count;
	c[2] = (intptr_t)timeout;
    //printf("push %x\n",(int)fds);
    if(count <= len){
        /* perform actual poll() */
        g_async_queue_push((GAsyncQueue *)q, c);
    }
    return count; /* pass this value to loop_dispatch */
}

MOSHEXPORT
void
mglib_loop_dispatch(void* fds, int count){
    gint r;
    r = g_main_context_check(g_main_context_default(),
                             999999, /* FIXME: */
                             (GPollFD* )fds,
                             count);
	//printf("dispatch: %d\n",r);
    if(r == TRUE){
        g_main_context_dispatch(g_main_context_default());
    }
}

MOSHEXPORT
int
mglib_add_timeout(unsigned int interval, void* func, void* data){
    return g_timeout_add(interval,func,data);
}

MOSHEXPORT
void*
mglib_timer_new(void){
    return g_timer_new();
}

MOSHEXPORT
void
mglib_timer_start(void* p){
    g_timer_start((GTimer*)p);
}

MOSHEXPORT
void
mglib_timer_elapsed(void* t, uintptr_t* out){
    gulong value;
    g_timer_elapsed((GTimer*)t,&value);
    *out = value;
}

