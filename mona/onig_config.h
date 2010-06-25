#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>
#include <stdlib.h>

void* GC_malloc(size_t size);
void* GC_realloc(void* ptr, size_t size);

#define HAVE_STRING_H 1
#define HAVE_STDARG_PROTOTYPES 1
#define SIZEOF_INT 4

#ifdef xmalloc
#undef xmalloc
#endif

#define xmalloc     malloc

#ifdef xrealloc
#undef xrealloc
#endif

#define xrealloc     GC_realloc

#ifdef xcalloc
#undef xcalloc
#endif

#define xcalloc(a, b)     GC_malloc(a * b)

#ifdef xfree
#undef xfree
#endif

void dont_free();
#define xfree     dont_free

#ifdef xalloca
#undef xalloca
#endif

#define xalloca   alloca

#endif /* CONFIG_H */
