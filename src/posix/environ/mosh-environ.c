#include "config.h"
#ifdef HAVE_EXECVE

#include "mosh-environ.h"

/* took from Ruby posix-spawn */

#ifdef __APPLE__
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char**environ;
#endif

void*
environ_getptr(void){
    return (void*)environ;
}

#endif
