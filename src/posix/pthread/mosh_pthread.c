#include "config.h"

#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

typedef uintptr_t (*ffithread_callback_t)(uintptr_t in0,uintptr_t in1,
                                          uintptr_t* out0,uintptr_t* out1);

typedef struct{
    uintptr_t in0;
    uintptr_t in1;
    uintptr_t func;
    int fd;
} ffithread_data;

static void*
ffithread(void* arg){
    ffithread_data* d = (ffithread_data *)arg;
    int fd = d->fd;
    uintptr_t in0 = d->in0;
    uintptr_t in1 = d->in1;
    ffithread_callback_t callback = (ffithread_callback_t)d->func;
    uintptr_t out0;
    uintptr_t out1;
    uintptr_t r;
    char buf[sizeof(uintptr_t)*2];
    free(d);

    for(;;){
        r = callback(in0,in1,&out0,&out1);
        memcpy(buf,&out0,sizeof(uintptr_t));
        memcpy(&buf[sizeof(uintptr_t)],&out1,sizeof(uintptr_t));
        write(fd, buf, sizeof(buf));
        if(!r){
            return NULL;
        }
    }
}

int
posix_invoke_ffithread(int fd,uintptr_t func,uintptr_t in0,uintptr_t in1){
    pthread_t bogus;
    pthread_attr_t attr;
    ffithread_data *d = malloc(sizeof(ffithread_data));

    d->fd = fd;
    d->in0 = in0;
    d->in1 = in1;
    d->func = func;

    pthread_attr_init(&attr);
    return pthread_create(&bogus, &attr, ffithread, d);
}
