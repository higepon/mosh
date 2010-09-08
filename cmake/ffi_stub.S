#include "config.h"

#ifdef ARCH_IA32
#include "ffi_stub_i386.S"
#endif

#ifdef ARCH_X86_64
#include "ffi_stub_x86_64.S"
#endif
