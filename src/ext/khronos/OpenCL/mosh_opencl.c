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
int /* success = 0 */
mcl_platform_profile(void* p, void* buf, int size, int* out_size){
    cl_int r;
    size_t osize;
    r = clGetPlatformInfo(p, CL_PLATFORM_PROFILE, size, buf, &osize);
    *out_size = osize;
    return r;
}

MOSHEXPORT
int /* success = 0 */
mcl_platform_version(void* p, void* buf, int size, int* out_size){
    cl_int r;
    size_t osize;
    r = clGetPlatformInfo(p, CL_PLATFORM_VERSION, size, buf, &osize);
    *out_size = osize;
    return r;
}

MOSHEXPORT
int /* success = 0 */
mcl_platform_name(void* p, void* buf, int size, int* out_size){
    cl_int r;
    size_t osize;
    r = clGetPlatformInfo(p, CL_PLATFORM_NAME, size, buf, &osize);
    *out_size = osize;
    return r;
}

MOSHEXPORT
int /* success = 0 */
mcl_platform_vendor(void* p, void* buf, int size, int* out_size){
    cl_int r;
    size_t osize;
    r = clGetPlatformInfo(p, CL_PLATFORM_VENDOR, size, buf, &osize);
    *out_size = osize;
    return r;
}

MOSHEXPORT
int /* success = 0 */
mcl_platform_extensions(void* p, void* buf, int size, int* out_size){
    cl_int r;
    size_t osize;
    r = clGetPlatformInfo(p, CL_PLATFORM_EXTENSIONS, size, buf, &osize);
    *out_size = osize;
    return r;
}

MOSHEXPORT
int /* fail = -1 */
mcl_platform_profile_size(void* p){
    size_t len;
    cl_int r;
    r = clGetPlatformInfo(p, CL_PLATFORM_PROFILE, 0, NULL, &len);
    if(r == CL_SUCCESS){
        return len;
    }else{
        return -1;
    }
}

MOSHEXPORT
int /* fail = -1 */
mcl_platform_version_size(void* p){
    size_t len;
    cl_int r;
    r = clGetPlatformInfo(p, CL_PLATFORM_VERSION, 0, NULL, &len);
    if(r == CL_SUCCESS){
        return len;
    }else{
        return -1;
    }
}

MOSHEXPORT
int /* fail = -1 */
mcl_platform_name_size(void* p){
    size_t len;
    cl_int r;
    r = clGetPlatformInfo(p, CL_PLATFORM_NAME, 0, NULL, &len);
    if(r == CL_SUCCESS){
        return len;
    }else{
        return -1;
    }
}

MOSHEXPORT
int /* fail = -1 */
mcl_platform_vendor_size(void* p){
    size_t len;
    cl_int r;
    r = clGetPlatformInfo(p, CL_PLATFORM_VENDOR, 0, NULL, &len);
    if(r == CL_SUCCESS){
        return len;
    }else{
        return -1;
    }
}

MOSHEXPORT
int /* fail = -1 */
mcl_platform_extensions_size(void* p){
    size_t len;
    cl_int r;
    r = clGetPlatformInfo(p, CL_PLATFORM_EXTENSIONS, 0, NULL, &len);
    if(r == CL_SUCCESS){
        return len;
    }else{
        return -1;
    }
}


MOSHEXPORT
int /* 0 = success */
mcl_clGetPlatformIDs(int count,cl_platform_id* out, int* ret){
    cl_uint num;
    cl_uint out_num;
    cl_int r;
    /* debug */
    num = count;
    r = clGetPlatformIDs(num,out,&out_num);
    *ret = out_num;
    return r;
}

MOSHEXPORT
int
mcl_platform_id_size(void){
    return sizeof(cl_platform_id);
}

MOSHEXPORT
int
mcl_platform_count(void){
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
