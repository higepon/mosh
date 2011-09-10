#include <config.h>

#if defined(HAVE_CL)||defined(HAVE_CL_CL_H)||defined(HAVE_OPENCL_CL_H)
#define __HAVE_CL 1
#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif
#endif

#ifdef __HAVE_CL

MOSHEXPORT
int
mcl_platformcount(void){
    /* took from AMD sample */
    cl_uint numPlatforms;
    cl_int status;
    status = clGetPlatformIDs(0, NULL, &numPlatforms);
    if(status != CL_SUCCESS){
        return 0;
    }
    return numPlatforms;
}

#endif /* __HAVE_CL */
