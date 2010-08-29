/* oniguruma compatibility header */
#define re_registers onig_re_registers /* these may conflict POSIX regex */
#define re_pattern_buffer onig_re_pattern_buffer
#ifdef GTEST_USES_POSIX_RE
#define ONIG_ESCAPE_REGEX_T_COLLISION
#endif
#include "oniguruma.h"

