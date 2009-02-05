#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(mosh, 0.0.8, higepon@users.sourceforge.jp)
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
  [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/."CFLAGS+=-m32  ./configure ABI=32 && make" for OSX])])


AC_ARG_ENABLE(profiler, [  --disable-profiler trun off profiler [default no]],,disable_profiler=no)
AC_MSG_CHECKING(whether to enable profiler)
if test x$disable_profiler = xyes; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_DEFINE(ENABLE_PROFILER, , enable -p option for profiling)
fi

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
  *)
    AC_DEFINE([MOSH_HOST_OS], ["unknown"], [define operating-system])
    ;;
esac


# Checks for cpu
AC_MSG_CHECKING([for supported architecture])
case "$host_cpu" in
i[[3456]]86|pentium)
        arch=i386
        MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer"
        AC_MSG_RESULT([$host_cpu])
        AC_DEFINE([ARCH_IA32], 1, [Define for the IA32 architecture.])
        ;;
x86?64*)
        arch=x86_64
        MOSH_OPTS="-O3 -momit-leaf-frame-pointer -fomit-frame-pointer"
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
AC_SUBST(MOSH_OPTS)

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
AC_CONFIG_SUBDIRS([gc-7.1alpha3 onig-5.7.0])
AC_OUTPUT(mosh_config lib/mosh/mysql.ss lib/mosh/config.ss doc/text/Download.txt)
