#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.62)
AC_INIT(mosh, 0.2.7, higepon@users.sourceforge.jp)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([src/VM.h])
AC_CONFIG_HEADER([src/config.h])

# we don't like default CFLAGS(-O2 -g).
AC_SUBST(CFLAGS)
if test "" = "$CFLAGS"; then
    CFLAGS=" "
fi

AC_SUBST(CXXFLAGS)
if test "" = "$CXXFLAGS"; then
    CXXFLAGS=" "
fi

AC_SUBST(MOSH_LIB_PATH, "${datadir}/mosh-$PACKAGE_VERSION")

# Checks for programs.
dnl AC_PROG_LIBTOOL # use LTLIBRARIES
dnl AC_LIBTOOL_DLOPEN # use dlopen
AM_PROG_AS
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_PROGS(NATURALDOCS,
  [naturaldocs NaturalDocs],
  [perl ~/NaturalDocs-1.4/NaturalDocs])

AC_ARG_WITH([r6rs-doc],
  AC_HELP_STRING([--with-r6rs-doc=PATH],
                 [build R6RS docs from TeX source at PATH]),
  AC_SUBST([R6RS_DOC_PATH], "$withval"),
  AC_SUBST([R6RS_DOC_PATH], "$HOME/Desktop/r6rs/document"))

# Prerequisites

LOCAL_CONFIGURE_ARGS="--host=$host --build=$build"

### GMP or MPIR(gmp API)

AC_CHECK_LIB(gmp, __gmpz_init, ,
  [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/.])])

### OpenSSL (optional)
have_openssl=no
AC_TRY_LINK([#include <openssl/opensslv.h>],
    [ return OPENSSL_VERSION_NUMBER; ],
    [
        have_openssl=1
        LIBS="-lssl -lcrypto $LIBS"
    ],
    [
        AC_MSG_WARN("libssl not found. OpenSSL Disabled...")
        have_openssl=0
    ])

HAVE_OPENSSL=$have_openssl
AC_DEFINE_UNQUOTED([HAVE_OPENSSL], $have_openssl, [Do we have openssl?])
AC_SUBST([HAVE_OPENSSL])

### Oniguruma 5
## we need oniguruma 5.x for unicode support

have_onig=no
local_onig_build=no
LOCAL_ONIG=""

# option0: use onig-config in PATH
if test $have_onig = no; then
    ONIGCONFIG=""
    AC_PATH_PROG(ONIGCONFIG,onig-config)
    if test "" = "$ONIGCONFIG"; then
        # put some warning here..
        ONIG_CFLAGS=""
        ONIG_LIBS=""
    else
        ONIG_CFLAGS=`$ONIGCONFIG --cflags`
        ONIG_LIBS=`$ONIGCONFIG --libs`
        saved_cflags=$CFLAGS
        saved_libs=$LIBS
        CFLAGS="$CFLAGS $ONIG_CFLAGS"
        LIBS="$LIBS $ONIG_LIBS"
        AC_CHECK_LIB([onig],[regexec],have_onig=yes,have_onig=no)
        CFLAGS=$saved_cflags
        LIBS=$saved_libs
    fi
fi

# option1 was disabled (not reliable in some Linux variants)
dnl # option1: try to link with current configuration
dnl if test $have_onig = "no" ; then
dnl     AC_CHECK_LIB([onig],[regexec],[LIBS="$LIBS -lonig" have_onig="yes"])
dnl fi

if test $have_onig = "no" ; then
    AC_MSG_ERROR([oniguruma not found, mosh requires oniguruma 5.x])
fi

AM_CONDITIONAL(BUILD_LOCAL_ONIG,test $local_onig_build = "yes")

AC_SUBST(ONIG_CFLAGS)
AC_SUBST(ONIG_LIBS)

# gtest requires POSIX regexp and this will conflict oniguruma
# We will define  -D ONIG_ESCAPE_REGEX_T_COLLISION for this

AC_SUBST(LOCAL_CONFIGURE_ARGS)

## ~prereq.

AC_ARG_ENABLE(profiler, [  --enable-profiler Build with profiler feature [default=yes]], ,[enable_profiler=yes])
AC_MSG_CHECKING(whether to enable profiler)
if test "$enable_profiler" = "yes"; then
    case $host in
        *mingw*)
        AC_MSG_ERROR([Profiling is not supported on MinGW hosts])
        ;;
        *)
        AC_DEFINE(ENABLE_PROFILER, 1, enable -p option for profiling)
        ;;
    esac
fi
AC_MSG_RESULT([$enable_profiler])

dnl AC_MSG_RESULT([$host_os])
dnl AC_MSG_RESULT([$host])
dnl AC_MSG_RESULT([$target])

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
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <tr1/unordered_map>
int main(int, char **)
{
    std::tr1::unordered_map<int, int> m;
    m.insert(std::make_pair(1, 2));
}
]])],
[have_tr1_hashes=1],
[have_tr1_hashes=0])

AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <ext/hash_map>
int main(int, char **)
{
    __gnu_cxx::hash_map<int, int> m;
    m.insert(std::make_pair(1, 2));
}
]])],
[have_ext_hashes=1],
[have_ext_hashes=0])
AC_MSG_RESULT([$have_ext_hashes])

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
  *cygwin*)
    AC_DEFINE([MOSH_HOST_OS], ["cygwin"], [define operating-system])
    ;;
  *)
    AC_DEFINE([MOSH_HOST_OS], ["unknown"], [define operating-system])
    ;;
esac

MOSH_GENERIC_OPTS="-O3"
MOSH_INTEL_OPTS="$MOSH_GENERIC_OPTS -momit-leaf-frame-pointer -fomit-frame-pointer"

# Checks for O/S specific features

has_gc_pthread=false
has_gc_darwin=false
has_gc_win32=false
AC_MSG_CHECKING([check threads])
case "$target_os" in
*darwin*)
  AC_DEFINE(GC_DARWIN_THREADS,1,[Define to use Darwin threads])
  has_gc_pthread=true
  has_gc_darwin=true
  ;;
*linux*)
  AC_DEFINE(GC_LINUX_THREADS,1,[Define to use Linux threads])
  AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc])
  has_gc_pthread=true
  ;;
freebsd*)
  AC_DEFINE(GC_FREEBSD_THREADS,1,[Define to use FreeBSD threads])
  has_gc_pthread=true
  ;;
netbsd*)
  AC_DEFINE(GC_NETBSD_THREADS,1,[Define to use NetBSD threads])
  has_gc_pthread=true
  ;;
cygwin)
  AC_DEFINE(GC_THREADS,1,[Define to use generic thread support])
  AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc])
  has_gc_pthread=true
  has_gc_win32=true
  ;;

mingw*)
  AC_DEFINE(GC_THREADS,1,[Define to use generic thread support]) # FIXME
  AC_DEFINE(_REENTRANT,1,[Define to use reentrant libc]) # FIXME
  has_gc_win32=true
  ;;
*)
  AC_MSG_ERROR([Please modify configure.ac for $target_os])
  ;;
esac

AM_CONDITIONAL(GC_PTHREAD, $has_gc_pthread)
AM_CONDITIONAL(GC_DARWIN, $has_gc_darwin)
AM_CONDITIONAL(GC_WIN32, $has_gc_win32)
# Checks for cpu
AC_MSG_CHECKING([for supported architecture])
MOSH_LDADD_ARCH="-ldl -lpthread"
case "$host_cpu" in
i[[3456]]86|pentium)
        arch=i386
        case "$target_os" in
        *darwin*)
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
         MOSH_OPTS="$MOSH_INTEL_OPTS"
         MOSH_LDADD_ARCH="-lpthread"
         ;;
       *netbsd*)
         MOSH_OPTS="$MOSH_INTEL_OPTS"
         MOSH_LDADD_ARCH="-lpthread"
         ;;
       *mingw*) #FIXME: now broken
         MOSH_OPTS="$MOSH_GENERIC_OPTS -mtune=generic -DMOSH_MINGW32 -D_UNICODE -DUNICODE -DWINVER=0x501 -D_WIN32_WINNT=0x501 -static-libgcc -static-libstdc++  -fwide-exec-charset=ucs-4le" #forcing WinXP & Gcc >= 4.5
         MOSH_LDADD_ARCH="-lshlwapi -lshell32 -lws2_32"
         ;;
       *cygwin*)
         MOSH_OPTS="$MOSH_INTEL_OPTS -mtune=generic -fwide-exec-charset=ucs-4le"
         MOSH_LDADD_ARCH="-ldl -lpthread"
	 ;;

        *)
          MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer" # -mfpmath=sse -msse3 -march=$host_cpu
          ;;
        esac

        AC_MSG_RESULT([$host_cpu])
        AC_DEFINE([ARCH_IA32], 1, [Define for the IA32 architecture.])
        AM_CONDITIONAL(FFI_I386, true)
        AM_CONDITIONAL(FFI_X86_64, false)
        ;;
	x86?64*|amd64) 
	arch=x86_64
        AC_DEFINE([ARCH_X86_64], 1, [Define for the AMD x86-64 architecture.])
        AM_CONDITIONAL(FFI_I386, false)
        AM_CONDITIONAL(FFI_X86_64, true)
        case "$target_os" in
                *darwin*)
                AC_MSG_RESULT([x86_64])
                MOSH_OPTS="-O3"
                ;;
                *freebsd*)
                AC_MSG_RESULT([x86_64(FreeBSD amd64)])
                MOSH_OPTS="$MOSH_INTEL_OPTS"
                MOSH_LDADD_ARCH="-lpthread"
                ;;
                *netbsd*)
                AC_MSG_RESULT([x86_64(NetBSD amd64)])
                MOSH_OPTS="$MOSH_INTEL_OPTS"
                MOSH_LDADD_ARCH="-lpthread"
                ;;
                *)
		MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer"
		AC_MSG_RESULT([x86_64])
		;;
	esac
	;;
	*)
        AC_MSG_RESULT([$host_cpu])
        MOSH_OPTS="-O3"
        AC_MSG_WARN([architecture $host_cpu is not supported, but may work other than FFI])
        AC_DEFINE([ARCH_UNKNOWN], 1, [Define for the unknown architecture.])
        AM_CONDITIONAL(FFI_I386, false)
        AM_CONDITIONAL(FFI_X86_64, false)
        ;;
	esac

AC_MSG_CHECKING([whether to enable MinGW support])
case "$host_os" in
*mingw*) #FIXME: now broken
        mingw32=true
        AC_MSG_RESULT([yes])
        AC_MSG_WARN([MinGW support with this configure is now broken. Use CMake instead])
	AC_CHECK_TOOL(WINDRES, windres)
	ac_configure_args="$ac_configure_args --disable-shared --enable-threads=win32 CFLAGS=\"$CFLAGS -DGC_THREADS\" win32_threads=true"

	;;
*)
        mingw32=false
        AC_MSG_RESULT([no])
	;;
esac
AM_CONDITIONAL(MINGW32, test x$mingw32 = xtrue)

AC_MSG_CHECKING([whether to enable kqueue support])
case "$host_os" in
freebsd*)
        kqueuestub=true
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_KQUEUE], 1, [BSD Kqueue support.])

	;;
*)
        kqueuestub=false
        AC_MSG_RESULT([no])
	;;
esac
AM_CONDITIONAL(KQUEUE, test x$kqueuestub = xtrue)

AC_MSG_CHECKING([whether to enable ptrace support])
case "$host_os" in
freebsd*)
        ptracestub_common=true
        AC_MSG_RESULT([yes(common)])
        AC_DEFINE([HAVE_PTRACE_COMMON], 1, [Common ptrace support.])

	;;
*)
        ptracestub_common=false
        AC_MSG_RESULT([no])
	;;
esac
AM_CONDITIONAL(PTRACE_COMMON, test x$ptracestub_common = xtrue)

AC_SUBST(MOSH_OPTS)
AC_SUBST(MOSH_LDADD_ARCH)


AC_ARG_WITH(mysql,
  AC_HELP_STRING([--with-mysql=FULL_PATH_TO_libmysqlclient]),
  [
  AC_SUBST(PATH_TO_MYSQLCLIENT, "$withval")
  ],
  [
  AC_SUBST(PATH_TO_MYSQLCLIENT, "libmysqlclient.so.16")
  ]
)

AC_ARG_ENABLE(developer, AS_HELP_STRING([--enable-developer],
                                        [Flags are enabled for Mosh Developer]) ,
                                        [enable_developer=true], [enable_developer=false])
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
    *cygwin* | *mingw* | amigados* | msdos* | rdos*)
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
AC_OUTPUT(src/mosh_config lib/mosh/mysql.ss lib/mosh/config.ss doc/text/Download.txt)
