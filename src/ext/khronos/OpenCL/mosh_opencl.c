#include <config.h>

#ifdef HAVE_CL_CL_H
#include <CL/cl.h>
#define __HAVE_CL 1
#endif

#ifdef HAVE_OPENCL_CL_H
#include <OpenCL/cl.h>
#define __HAVE_CL 1
#endif

#ifdef __HAVE_CL

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
