#include <gc.h>

#include "boehmgc-stubs.h"

void
register_disappearing_link(void** link, void* obj){
    GC_GENERAL_REGISTER_DISAPPEARING_LINK(link,obj);
}

void
gcollect(void){
    GC_gcollect();
}
