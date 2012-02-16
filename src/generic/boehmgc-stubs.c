#include <gc.h>

#include "generic/boehmgc-stubs.h"

void*
create_weak_vector(int count){
    return GC_MALLOC_ATOMIC(sizeof(void*)*count);
}

/* Of corse, we have no destroy_weak_vector() */

void*
weak_vector_ref(void** wv,int ref){
    return wv[ref];
}

void
weak_vector_set(void** wv,int ref,void* val){
    wv[ref] = val;
}

void
register_disappearing_link_wv(void** wv,int ref,void* obj){
    register_disappearing_link(&wv[ref],obj);
}

typedef struct{
    GC_finalization_proc fn;
    void* data;
    GC_finalization_proc oldfn;
    void* olddata;
}chaincall;

static void
chaincall_handler(void* obj,void* ptr){
    chaincall* cc = (chaincall *)ptr;
    GC_finalization_proc fn = cc->fn; 
    GC_finalization_proc oldfn = cc->oldfn;
    void* data = cc->data;
    void* olddata = cc->olddata;
    fn(obj,data);
    oldfn(obj,olddata);
}

static chaincall*
chaincaller(void* fn,void *data,void* oldfn,void* olddata){
    chaincall *cc;
    cc = (chaincall *)GC_MALLOC(sizeof(chaincall));
    cc->fn = (GC_finalization_proc)fn;
    cc->data = data;
    cc->oldfn = (GC_finalization_proc)oldfn;
    cc->olddata = olddata;
    return cc;
}

// FIXME: We always use IGNORE_SELF version of FINALIZER.
void
register_finalizer(void* obj,void *fn,void *cd){
    void* oldfn;
	void* olddata;
    chaincall* cc;
    GC_REGISTER_FINALIZER_IGNORE_SELF(obj, (GC_finalization_proc)fn
            ,cd, (GC_finalization_proc *)&oldfn, &olddata);
    if(oldfn){
        cc = chaincaller(fn,cd,oldfn,olddata);
        GC_REGISTER_FINALIZER_IGNORE_SELF(obj, chaincall_handler,
                cc,(GC_finalization_proc *)&oldfn, &olddata);
    }
}

void
register_disappearing_link(void** link, void* obj){
    GC_GENERAL_REGISTER_DISAPPEARING_LINK(link,obj);
}

void
gcollect(void){
    GC_gcollect();
}

void
genable_incremental(void){
    GC_enable_incremental();
}

int
gcurrent_size(void){
    return GC_get_heap_size();
}

int
gfree_size(void){
    return GC_get_free_bytes();
}

void
gset_time_limit(int time){
    GC_time_limit = time;
}
