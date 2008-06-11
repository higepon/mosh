#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(mosh, 0.0.4, higepon@users.sourceforge.jp)
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([VM.h])
AC_CONFIG_HEADER([config.h])

AC_SUBST(MOSH_LIB_PATH, "${datadir}/mosh")

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
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_PROG(GOSH, gosh, gosh, no)
if test "$GOSH" != "gosh"
    then AC_MSG_ERROR([gosh not found. Gauche (http://practical-scheme.net/gauche/) is required to build.])
fi

AC_ARG_ENABLE(profiler, [  --disable-profiler trun off profiler [default no]],,disable_profiler=no)
AC_MSG_CHECKING(whether to enable profiler)
if test x$disable_profiler = xyes; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_DEFINE(ENABLE_PROFILER, , enable -p option for profiling)
fi


# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h sys/time.h utmp.h])

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

AC_CONFIG_FILES([Makefile])
AC_CONFIG_SUBDIRS([gc-7.1alpha3 onig-5.7.0])
AC_OUTPUT
