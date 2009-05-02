#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.63)
AC_INIT(mosh, 0.1.0, higepon@users.sourceforge.jp)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([VM.h])
AC_CONFIG_HEADER([config.h])


AC_SUBST(MOSH_LIB_PATH, "${datadir}/mosh/$PACKAGE_VERSION")


# we don't like default CFLAGS(-O2 -g).
AC_SUBST(CFLAGS)
if test "" = "$CFLAGS"; then
    CFLAGS=" "
fi

AC_SUBST(CXXFLAGS)
if test "" = "$CXXFLAGS"; then
    CXXFLAGS=" "
fi

# Checks for programs.
dnl AC_PROG_LIBTOOL # use LTLIBRARIES
dnl AC_LIBTOOL_DLOPEN # use dlopen
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_PROG(GOSH, gosh, gosh, no)
dnl if test "$GOSH" != "gosh"
dnl     then AC_MSG_ERROR([gosh not found. Gauche (http://practical-scheme.net/gauche/) is required to build.])
dnl fi
AC_CHECK_LIB(gmp, __gmpz_init, ,
  [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/."For OSX, install GNU MP with "CFLAGS+=-m32 ./configure ABI=32 && make"])])


AC_ARG_ENABLE(profiler, [  --enable-profiler turn off profiler [default=yes]], ,[enable_profiler=yes])
AC_MSG_CHECKING(whether to enable profiler)
if test "$enable_profiler" = "yes"; then
  AC_DEFINE(ENABLE_PROFILER, 1, enable -p option for profiling)
fi

AC_MSG_RESULT([$host_os])
AC_MSG_RESULT([$host])
AC_MSG_RESULT([$target])

case $host in
  *darwin*)
  SHLIB_SO_LDFLAGS="-dynamiclib"
  ;;
  *)
  SHLIB_SO_LDFLAGS="-shared"
  ;;
esac
AC_SUBST(SHLIB_SO_LDFLAGS)

# Checks for libraries.
have_tr1_hashes=0
have_ext_hashes=0
AC_LANG([C++])
AC_MSG_CHECKING([for hashed associative containers])
AC_COMPILE_IFELSE([
#include <tr1/unordered_map>
int main(int, char **)
{
    std::tr1::unordered_map<int, int> m;
    m.insert(std::make_pair(1, 2));
}
],
[have_tr1_hashes=1],
[have_tr1_hashes=0])

AC_COMPILE_IFELSE([
#include <ext/hash_map>
int main(int, char **)
{
    __gnu_cxx::hash_map<int, int> m;
    m.insert(std::make_pair(1, 2));
}
],
[have_ext_hashes=1],
[have_ext_hashes=0])
AC_MSG_RESULT([$have_ext_hashes])

dnl AC_MSG_CHECKING([sse])
dnl AC_COMPILE_IFELSE([
dnl #include <xmmintrin.h>
dnl int main(int, char **)
dnl {
dnl     __m128 v;v=_mm_sub_ss(v,v);
dnl }
dnl ],
dnl [have_sse=1],
dnl [have_sse=0])

dnl if test x$have_sse = x1; then
dnl   AC_MSG_RESULT(yes)
dnl else
dnl   AC_MSG_RESULT(no)
dnl fi

dnl AC_MSG_CHECKING([sse2])
dnl AC_COMPILE_IFELSE([
dnl #include <emmintrin.h>
dnl int main(int, char **)
dnl {
dnl    __m128i v;v=_mm_add_epi32(v,v);
dnl }
dnl ],
dnl [have_sse2=1],
dnl [have_sse2=0])

dnl if test x$have_sse2 = x1; then
dnl   AC_MSG_RESULT(yes)
dnl else
dnl   AC_MSG_RESULT(no)
dnl fi

dnl AC_MSG_CHECKING([sse3])
dnl AC_COMPILE_IFELSE([
dnl #include <emmintrin.h>
dnl int main(int, char **)
dnl {
dnl     __m128d v;v=_mm_addsub_pd(v,v);
dnl }
dnl ],
dnl [have_sse3=1],
dnl [have_sse3=0])

dnl if test x$have_sse3 = x1; then
dnl   AC_MSG_RESULT(yes)
dnl else
dnl   AC_MSG_RESULT(no)
dnl fi

dnl if test x$have_sse3 = x1; then
dnl     MOSH_SSE_OPTS=" -msse3"
dnl else
dnl if test x$have_sse2 = x1; then
dnl     MOSH_SSE_OPTS=" -msse2"
dnl else
dnl if test x$have_sse = x1; then
dnl     MOSH_SSE_OPTS=" -msse"
dnl else
dnl     MOSH_SSE_OPTS=""
dnl fi
dnl fi
dnl fi


HAVE_TR1_HASHES=$have_tr1_hashes
AC_DEFINE_UNQUOTED([HAVE_TR1_HASHES], $have_tr1_hashes, [Do we have std::tr1:: hashes?])
AC_SUBST([HAVE_TR1_HASHES])
HAVE_EXT_HASHES=$have_ext_hashes
AC_DEFINE_UNQUOTED([HAVE_EXT_HASHES], $have_ext_hashes, [Do we have __gnu_cxx:: hashes?])
AC_SUBST([HAVE_EXT_HASHES])

# Checks for OS
case "$target_os" in
  *linux*)
    AC_DEFINE([MOSH_HOST_OS], ["linux"], [define operating-system])
    ;;
  *bsd*)
    AC_DEFINE([MOSH_HOST_OS], ["bsd"], [define operating-system])
    ;;
  *darwin*)
    AC_DEFINE([MOSH_HOST_OS], ["darwin"], [define operating-system])
    ;;
  *mingw*)
    AC_DEFINE([MOSH_HOST_OS], ["win32"], [define operating-system])
    ;;
  *)
    AC_DEFINE([MOSH_HOST_OS], ["unknown"], [define operating-system])
    ;;
esac

#MOSH_INTEL_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer"
MOSH_GENERIC_OPTS="-O3"
MOSH_INTEL_OPTS="$MOSH_GENERIC_OPTS -momit-leaf-frame-pointer -fomit-frame-pointer"

#MOSH_INTEL_OPTS="-O3 -momit-leaf-frame-pointer -pg"

# Checks for cpu
AC_MSG_CHECKING([for supported architecture])
MOSH_LDADD_ARCH="-ldl -lpthread"
case "$host_cpu" in
i[[3456]]86|pentium)
        arch=i386
        case "$target_os" in
        *darwin*)
#          MOSH_LDADD_ARCH="$MOSH_LDADD_ARCH -framework CoreFoundation"
          if test -n "`system_profiler |grep 'Intel Core 2'`" ; then
             MOSH_OPTS="$MOSH_INTEL_OPTS -march=pentium-m -msse2 -mfpmath=sse"
          else
             MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu"
          fi
          ;;
        *linux*)
          if test -n "`grep 'Core(TM)2 CPU' /proc/cpuinfo`" ; then
             MOSH_OPTS="$MOSH_INTEL_OPTS -march=pentium-m -msse3 -mfpmath=sse"
          else
            if test -n "`grep 'ssse3' /proc/cpuinfo`" ; then
               MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu -msse3 -mfpmath=sse"
            else
              if test -n "`grep 'sse2' /proc/cpuinfo`" ; then
                MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu -msse2 -mfpmath=sse"
              else
                if test -n "`grep 'sse' /proc/cpuinfo`" ; then
                  MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu -msse -mfpmath=sse"
                else
                  MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu"
                fi
              fi
            fi
          fi
          ;;
       *freebsd*)
         MOSH_OPTS="$MOSH_INTEL_OPTS -march=$host_cpu"
         MOSH_LDADD_ARCH="-lpthread"
         ;;
       *mingw*)
	 MOSH_OPTS="$MOSH_GENERIC_OPTS -DMOSH_MINGW32 -D_UNICODE -DUNICODE -DWINVER=0x501 -D_WIN32_WINNT=0x501"
         MOSH_LDADD_ARCH="-lshlwapi -lshell32 -lws2_32"
         ;;
        *)
          MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer -march=$host_cpu" # -mfpmath=sse -msse3 -march=$host_cpu
          ;;
        esac

        AC_MSG_RESULT([$host_cpu])
        AC_DEFINE([ARCH_IA32], 1, [Define for the IA32 architecture.])
        ;;
x86?64*)
        arch=x86_64
        MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer -msse3 -mfpmath=sse"
        AC_MSG_RESULT([x86_64])
        AC_DEFINE([ARCH_X86_64], 1, [Define for the AMD x86-64 architecture.])
        ;;
*)
        AC_MSG_RESULT([$host_cpu])
        MOSH_OPTS="-O3"
        AC_MSG_WARN([architecture $host_cpu is not supported, but may work other than FFI])
        AC_DEFINE([ARCH_UNKNOWN], 1, [Define for the AMD x86-64 architecture.])
        ;;
esac

AC_MSG_CHECKING([whether to enable MinGW support])
case "$host_os" in
*mingw*)
        mingw32=true
        AC_MSG_RESULT([yes])
	AC_CHECK_TOOL(WINDRES, windres)
	;;
*)
        mingw32=false
        AC_MSG_RESULT([no])
	;;
esac
AM_CONDITIONAL(MINGW32, test x$mingw32 = xtrue)

AC_SUBST(MOSH_OPTS)
AC_SUBST(MOSH_LDADD_ARCH)


AC_ARG_WITH(mysql,
  AC_HELP_STRING([--with-mysql=FULL_PATH_TO_libmysqlclient]),
  [
  AC_SUBST(PATH_TO_MYSQLCLIENT, "$withval")
  ],
  [
  AC_SUBST(PATH_TO_MYSQLCLIENT, "libmysqlclient.so.15")
  ]
)

AC_ARG_ENABLE(developer, AS_HELP_STRING([--enable-developer],
                                        [Flags are enabled for Mosh Developer])
             , [enable_developer=true], [enable_developer=false])
AM_CONDITIONAL(DEVELOPER, test x$enable_developer = xtrue)


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h sys/time.h utmp.h tr1/unordered_map ext/hash_map unistd.h])
AC_CHECK_HEADERS([sys/types.h sys/wait.h sys/stat.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_C_BIGENDIAN
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T
AC_C_VOLATILE

# Checks for library functions.
AC_FUNC_ALLOCA
AC_CHECK_FUNCS([gettimeofday memmove memset strtol])

# Check system type
# todo: added another system type
case "$target_in" in
    cygwin* | mingw* | amigados* | msdos* | rdos*)
        AC_DEFINE(LINE_FEED_CODE_CRLF, 1, system line feed code is CRLF)
        ;;
    mac*)
        AC_DEFINE(LINE_FEED_CODE_CR, 1, system line feed code is CR)
        ;;
    *)
        AC_DEFINE(LINE_FEED_CODE_LF, 1, system line feed code is LF)
        ;;
esac

AC_CONFIG_FILES([Makefile])
AC_CONFIG_SUBDIRS([gc-7.1 onig-5.7.0])
AC_OUTPUT(mosh_config lib/mosh/mysql.ss lib/mosh/config.ss doc/text/Download.txt)
