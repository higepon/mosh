ACLOCAL_AMFLAGS = -I m4
# GLOBAL CFLAGS

GC_CFLAGS = \
	    -DGC_THREADS \
	    -DLARGE_CONFIG=1 \
	    -DTHREAD_LOCAL_ALLOC=1 \
	    -DHAS_X86_THREAD_STATE64___RAX=1 \
	    -DATOMIC_UNCOLLECTABLE=1 \
	    -DGC_BUILTIN_ATOMIC=1 \
	    -DNO_EXECUTE_PERMISSION=1 \
	    -DALL_INTERIOR_POINTERS=1 \
	    -DJAVA_FINALIZATION=1 \
	    -DGC_GCJ_SUPPORT=1 \
	    -DUSE_I686_PREFETCH \
		-DDONT_ADD_BYTE_AT_END \
		-DHANDLE_FORK \
		-DGC_NOT_DLL 

GLOBAL_CFLAGS =  $(ONIG_CFLAGS) $(GMP_CFLAGS) \
		 -g \
		 -Wall \
		 -I src -I $(includedir) \
		 -D MONA_SCHEME -D USE_BOEHM_GC \
		 -D MOSH_LIB_PATH="\"${datadir}/$(INSTALLNAME)-$(PACKAGE_VERSION)\"" \
		 -D_FILE_OFFSET_BITS=64 -Wall -pipe $(DEBUG_FLAGS) \
		 $(GC_CFLAGS) \
		 $(MOSH_OPTS) # temp -Wno-deprecated

GLOBAL_CXXFLAGS = -Wno-deprecated -std=c++11

LIBS += $(ONIG_LIBS) $(GMP_LIBS)

include automake/dist-git-revision.mk

if DEVELOPER
GIT_IS_DIRTY=$(shell git diff --quiet HEAD || echo "dirty")
GIT_COMMIT_REVISION=$(shell git describe --always)$(GIT_IS_DIRTY)
GIT_COMMIT_UNIXTIME=$(shell git cat-file commit HEAD|grep 'committer'|  awk '{ print $$(NF-1); }')
GIT_COMMIT_DATE=$(shell date -d "1970/01/01 09:00:00 $(GIT_COMMIT_UNIXTIME) seconds" "+%Y/%m/%d %H:%M:%S")
else
GIT_COMMIT_REVISION="$(DIST_GIT_REVISION)"
GIT_COMMIT_DATE="$(DIST_GIT_COMMIT_DATE)"
endif

GLOBAL_CFLAGS+=-D GIT_COMMIT_REVISION="\"$(GIT_COMMIT_REVISION)\"" -D GIT_COMMIT_DATE="\"$(GIT_COMMIT_DATE)\""

if DEVELOPER
DEBUG_FLAGS=-DDEBUG_VERSION
endif

AM_CFLAGS   = $(GLOBAL_CFLAGS)
AM_CXXFLAGS = $(GLOBAL_CFLAGS) $(GLOBAL_CXXFLAGS)

# Boehm GC
BOEHM_GC_DIR = extlibs/gc-cvs

# for GTEST
GTEST_DIR     = $(top_srcdir)/gtest
USER_DIR      = .
TESTS         = # used later

man_MANS = doc/mosh.1 doc/mosh_config.1

if DEVELOPER
bin_PROGRAMS = mosh nmosh test_port test_fasl test_ffi test_object test_vm test_compat test_getopt test_thread
else
bin_PROGRAMS = mosh nmosh
endif

bin_SCRIPTS = src/mosh_config

# mosh core sources

MOSH_CORE_SRCS = \
src/Code.h  src/SharedReference.h                                      \
src/NonGenerativeRTDs.h                                                \
src/HeapObject.h                                                       \
src/Object-inl.h                                                       \
src/Object-accessors.h                                                 \
src/Pair-inl.h                                                         \
src/VM-inl.h                                                           \
src/match.h                                                            \
src/baselib.h                                                          \
src/CProcedure.h                                                       \
src/Instruction.h                                                      \
src/Callable.h                                                         \
src/GenericMap.h                                                       \
src/BinaryInputPort.h                                                  \
src/BinaryOutputPort.h                                                 \
src/scheme.h                                                           \
src/ByteVector.h                                                       \
src/HashTable.h                                                        \
src/Pair.h                                                             \
src/ExecutableMemory.h                                                 \
src/LineBufferedFileBinaryOutputPort.h                                 \
src/Stack.h                                                            \
src/Array.h                                                            \
src/Gloc.h                         src/BinaryInputOutputPort.h         \
src/Box.h                          src/Port.h                          \
src/BinaryPort.h                   src/OutputPort.h                    \
src/Codec.h                                                            \
src/Transcoder.h                   src/Transcoder.cpp                  \
src/ProcedureMacro.h                                                   \
src/EqvHashTable.h                 src/EqvHashTable.cpp                \
src/Compnum.h                      src/Compnum.cpp                     \
src/Fixnum.h                       src/Fixnum.cpp                      \
src/Closure.h                      src/Closure.cpp                     \
src/UtilityProcedures.h            src/UtilityProcedures.cpp           \
src/ProcessProcedures.h            src/ProcessProcedures.cpp           \
src/Reader.h                       src/Reader.cpp                      \
src/NumberReader.h                 src/NumberReader.cpp                \
src/scheme.h                       src/scheme.cpp                      \
src/Regexp.h                       src/Regexp.cpp                      \
src/Symbol.h                       src/Symbol.cpp                      \
src/VM.h                           src/VM.cpp                          \
src/VM-Profiler.cpp                src/VM-Run.cpp                      \
src/VM-Run.h                       src/Continuation.h                  \
src/Scanner.h                      src/Scanner.cpp                     \
src/NumberScanner.h                src/NumberScanner.cpp               \
src/ByteArrayBinaryInputPort.h     src/ByteArrayBinaryInputPort.cpp    \
src/ByteArrayBinaryOutputPort.h    src/ByteArrayBinaryOutputPort.cpp   \
src/ScannerHelper.h                src/ScannerHelper.cpp               \
src/Reader.tab.hpp                 src/Reader.tab.cpp                  \
src/NumberReader.tab.hpp           src/NumberReader.tab.cpp            \
src/FileBinaryInputPort.h          src/FileBinaryInputPort.cpp         \
src/BufferedFileBinaryInputPort.h  src/BufferedFileBinaryInputPort.cpp \
src/FileBinaryOutputPort.h         src/FileBinaryOutputPort.cpp        \
src/BufferedFileBinaryOutputPort.h src/BufferedFileBinaryOutputPort.cpp\
src/CustomBinaryInputPort.h        src/CustomBinaryInputPort.cpp       \
src/CustomTextualInputPort.h       src/CustomTextualInputPort.cpp      \
src/CustomTextualOutputPort.h      src/CustomTextualOutputPort.cpp     \
src/CustomTextualInputOutputPort.h src/CustomTextualInputOutputPort.cpp\
src/CustomBinaryInputOutputPort.h  src/CustomBinaryInputOutputPort.cpp \
src/TextualOutputPort.h            src/TextualOutputPort.cpp           \
src/TextualInputPort.cpp           src/TextualInputPort.h              \
src/TranscodedTextualInputPort.cpp src/TranscodedTextualInputPort.h    \
src/TranscodedTextualOutputPort.cpp src/TranscodedTextualOutputPort.h  \
src/StringTextualOutputPort.h      src/StringTextualOutputPort.cpp     \
src/StringTextualInputPort.h       src/StringTextualInputPort.cpp      \
src/Codec.cpp                      src/Codec.h                         \
src/UTF8Codec.cpp                  src/UTF8Codec.h                     \
src/UTF16Codec.cpp                 src/UTF16Codec.h                    \
src/UTF32Codec.cpp                 src/UTF32Codec.h                    \
src/EqHashTable.h                  src/EqHashTable.cpp                 \
src/SString.h                      src/SString.cpp                     \
src/GenericHashTable.h             src/GenericHashTable.cpp            \
src/Object.h                       src/Object.cpp                      \
src/CompilerProcedures.cpp         src/CompilerProcedures.h            \
src/HashTableProcedures.cpp        src/HashTableProcedures.h           \
src/RegexpProcedures.cpp           src/RegexpProcedures.h              \
src/ArrayProcedures.cpp            src/ArrayProcedures.h               \
src/StringProcedures.cpp           src/StringProcedures.h              \
src/PortProcedures.cpp             src/PortProcedures.h                \
src/ListProcedures.cpp             src/ListProcedures.h                \
src/ErrorProcedures.cpp            src/ErrorProcedures.h               \
src/ArithmeticProcedures.cpp       src/ArithmeticProcedures.h          \
src/FlonumProcedures.cpp           src/FlonumProcedures.h              \
src/BitwiseProcedures.cpp          src/BitwiseProcedures.h             \
src/ByteVectorProcedures.cpp       src/ByteVectorProcedures.h          \
src/SocketProcedures.cpp           src/SocketProcedures.h              \
src/Equivalent.h                   src/Equivalent.cpp                  \
src/ucs4string.h                   src/ucs4string.cpp                  \
src/Vector.h                       src/Vector.cpp                      \
src/Fasl.h \
src/FaslReader.h                   src/FaslReader.cpp                  \
src/FaslWriter.h                   src/FaslWriter.cpp                  \
src/Arithmetic.h                   src/Arithmetic.cpp                  \
src/Ratnum.h                       src/Ratnum.cpp                      \
src/Bignum.h                       src/Bignum.cpp                      \
src/Flonum.h                       src/Flonum.cpp                      \
src/CodeBuilder.h                  src/CodeBuilder.cpp                 \
src/FixnumProcedures.h             src/FixnumProcedures.cpp            \
src/Latin1Codec.h                  src/Latin1Codec.cpp                 \
src/FFI.h                          src/FFI.cpp                         \
src/FFIProcedures.h                src/FFIProcedures.cpp               \
src/CustomBinaryOutputPort.h       src/CustomBinaryOutputPort.cpp      \
src/FileBinaryInputOutputPort.h    src/FileBinaryInputOutputPort.cpp   \
src/SocketBinaryInputOutputPort.h  src/SocketBinaryInputOutputPort.cpp \
src/TranscodedTextualInputOutputPort.h  src/TranscodedTextualInputOutputPort.cpp   \
src/BufferedFileBinaryInputOutputPort.h  src/BufferedFileBinaryInputOutputPort.cpp \
src/TestingFileBinaryOutputPort.h src/TestingFileBinaryInputPort.h                 \
src/TestingVM.h src/OSConstants.h \
src/getoptU.h src/getoptU.cpp  \
src/StandardOutputPort.h \
src/StandardErrorPort.h \
src/StandardInputPort.h \
src/BlockBufferedFileBinaryOutputPort.h \
src/BlockBufferedFileBinaryInputOutputPort.h \
src/LineBufferedFileBinaryInputOutputPort.h \
src/MultiVMProcedures.h src/MultiVMProcedures.cpp \
src/OSCompatSocket.h src/OSCompatSocket.cpp \
src/OSCompatThread.h src/OSCompatThread.cpp \
src/VMFactory.cpp src/VMFactory.h \
src/SimpleStruct.h \
src/OSCompat.h src/OSCompat.cpp \
src/oniguruma_compat.h \
src/stubs.cpp \
src/generic/boehmgc-stubs.c src/generic/boehmgc-stubs.h \
src/posix/terminal/terminal.c \
src/posix/spawn/posixspawn.c src/posix/environ/mosh-environ.c \
src/posix/fd/posix_fd.c src/posix/poll/posix_poll.c \
src/posix/socket/posix_socket.c src/posix/sigchld_handler/sigchld_handler.c \
src/posix/debugee/posix_debugee.c \
src/bsd/kqueue/kqueue_stubs.c \
src/posix/wait3/wait3.c \
src/posix/pthread/mosh_pthread.c \
src/posix/debugee/posix_debugee.h src/posix/environ/mosh-environ.h \
src/posix/fd/posix_fd.h src/posix/poll/posix_poll.h \
src/posix/pthread/mosh_pthread.h src/posix/ptrace/ptrace_common.h \
src/posix/sigchld_handler/sigchld_handler.h src/posix/socket/posix_socket.h \
src/posix/spawn/posixspawn.h src/posix/terminal/mosh_terminal.h \
src/posix/wait3/wait3.h \
src/win/include/getopt.h src/win/include/gettimeofday.h src/win32/aio_win32.h \
src/bsd/kqueue/kqueue_stubs.h 

if FFI_I386
MOSH_CORE_SRCS += src/ffi_stub_i386.S
endif

if FFI_X86_64
MOSH_CORE_SRCS += src/ffi_stub_x86_64.S
endif

if MINGW32
MOSH_CORE_SRCS += src/win32/process.c
endif

if PTRACE_COMMON
MOSH_CORE_SRCS += src/posix/ptrace/ptrace_common.c
endif

GC_SRCS = \
$(BOEHM_GC_DIR)/allchblk.c \
$(BOEHM_GC_DIR)/alloc.c \
$(BOEHM_GC_DIR)/blacklst.c \
$(BOEHM_GC_DIR)/checksums.c \
$(BOEHM_GC_DIR)/dbg_mlc.c \
$(BOEHM_GC_DIR)/dyn_load.c \
$(BOEHM_GC_DIR)/finalize.c \
$(BOEHM_GC_DIR)/gc_dlopen.c \
$(BOEHM_GC_DIR)/gcj_mlc.c \
$(BOEHM_GC_DIR)/headers.c \
$(BOEHM_GC_DIR)/malloc.c \
$(BOEHM_GC_DIR)/mallocx.c \
$(BOEHM_GC_DIR)/mark.c \
$(BOEHM_GC_DIR)/mark_rts.c \
$(BOEHM_GC_DIR)/misc.c \
$(BOEHM_GC_DIR)/new_hblk.c \
$(BOEHM_GC_DIR)/obj_map.c \
$(BOEHM_GC_DIR)/os_dep.c \
$(BOEHM_GC_DIR)/mach_dep.c \
$(BOEHM_GC_DIR)/reclaim.c \
$(BOEHM_GC_DIR)/specific.c \
$(BOEHM_GC_DIR)/typd_mlc.c \
$(BOEHM_GC_DIR)/backgraph.c \
$(BOEHM_GC_DIR)/ptr_chck.c \
$(BOEHM_GC_DIR)/gc_badalc.cc \
$(BOEHM_GC_DIR)/thread_local_alloc.c

GC_PTHREAD_SRCS = \
$(BOEHM_GC_DIR)/pthread_start.c \
$(BOEHM_GC_DIR)/pthread_support.c \
$(BOEHM_GC_DIR)/pthread_stop_world.c 

GC_DARWIN_SRCS = \
$(BOEHM_GC_DIR)/darwin_stop_world.c 

GC_WIN32_SRCS = \
$(BOEHM_GC_DIR)/win32_threads.c 

if GC_PTHREAD
GC_SRCS += $(GC_PTHREAD_SRCS)
endif

if GC_DARWIN
GC_SRCS += $(GC_DARWIN_SRCS)
endif

if GC_WIN32
GC_SRCS += $(GC_WIN32_SRCS)
endif

MOSH_CORE_SRCS += $(GC_SRCS)

noinst_LIBRARIES = libmosh_main.a libnmosh_main.a libmosh_core_for_test.a

# mosh
# -O3 causes miss compile of NumberReader.tab.cpp. (newest gcc?)
# see http://d.hatena.ne.jp/higepon/20081114/1226645373
libmosh_main_a_SOURCES  = src/main.cpp src/psyntax_mosh_image.cpp
mosh_SOURCES = $(MOSH_CORE_SRCS)
mosh_LDADD = libmosh_main.a $(REGEXP_LIB) $(MOSH_LDADD_ARCH)
mosh_DEPENDENCIES = libmosh_main.a

libnmosh_main_a_SOURCES = src/main.cpp src/nmosh_image.cpp 
libnmosh_main_a_CXXFLAGS = $(GLOBAL_CFLAGS) $(GLOBAL_CXXFLAGS) -D WITH_NMOSH_DEFAULTS
nmosh_SOURCES = $(MOSH_CORE_SRCS)
nmosh_LDADD = libnmosh_main.a $(REGEXP_LIB) $(MOSH_LDADD_ARCH)
nmosh_DEPENDENCIES = libnmosh_main.a

libmosh_core_for_test_a_SOURCES = $(MOSH_CORE_SRCS)

if MINGW32
nmosh_LDADD += mosh-icon.o
nmosh_DEPENDENCIES += mosh-icon.o
mosh-icon.o : win/mosh.rc misc/logo/mosh.ico
	-mkdir -p $(top_builddir)/misc/logo
	-cp -p $(top_srcdir)/misc/logo/mosh.ico $(top_builddir)/misc/logo
	$(WINDRES) $< -o mosh-icon.o
endif 

# if BUILD_LOCAL_ONIG
# include extlibs/onig-5.9.2.mk
# MOSH_CORE_SRCS += $(ONIG_SRCS)
# endif

BUILT_SOURCES = src/Object-accessors.h src/Instruction.h src/Reader.tab.hpp src/NumberReader.tab.hpp src/Reader.tab.cpp src/NumberReader.tab.cpp src/Scanner.cpp src/NumberScanner.cpp src/cprocedures.cpp src/labels.cpp src/OSConstants.h

include automake/corelibs.mk

# FIXME: should be autogenerated
.PHONY: mosh-plugins
MOSH_PLUGINS=

if MOSH_OPENCL
MOSH_PLUGINS+= $(top_builddir)/plugins/mosh_opencl.mplg
mosh-plugins: $(top_builddir)/plugins/mosh_opencl.mplg
$(top_builddir)/plugins/mosh_opencl.mplg: $(top_srcdir)/src/ext/khronos/OpenCL/mosh_opencl.c
	mkdir -p $(top_builddir)/plugins
	$(CC) -std=c99 -Wall -fPIC $(SHLIB_SO_LDFLAGS) $(GLOBAL_CFLAGS) $(CL_CFLAGS) -DMOSHEXPORT= $(CL_LIBS) $< -o $@
endif

if MOSH_CURSES
MOSH_PLUGINS+= $(top_builddir)/plugins/mosh_curses.mplg
mosh-plugins: $(top_builddir)/plugins/mosh_curses.mplg
$(top_builddir)/plugins/mosh_curses.mplg: $(top_srcdir)/src/ext/curses/mosh_curses.c
	mkdir -p $(top_builddir)/plugins
	$(CC) -std=c99 -Wall -fPIC $(SHLIB_SO_LDFLAGS) $(GLOBAL_CFLAGS) -DMOSHEXPORT= $< $(CURSES_LIB) -o $@
endif

#mosh_core_fasl_libraries = $(mosh_core_libraries:.ss=.ss.fasl)
scmlibdir = ${datadir}/$(INSTALLNAME)-$(PACKAGE_VERSION)
scmlib_DATA = src/all-tests.scm
nobase_scmlib_DATA = ${mosh_core_libraries} lib/mosh/config.ss ${MOSH_PLUGINS} #${mosh_core_fasl_libraries}

INCLUDES       = -I $(top_srcdir)/$(BOEHM_GC_DIR)/include -I $(top_srcdir)/$(BOEHM_GC_DIR)/include/gc -I $(top_srcdir)/$(BOEHM_GC_DIR)/libatomic_ops/src -I$(top_srcdir)/src
EXTRA_DIST     = \
boot/vm.scm boot/baselib boot/compiler.scm \
boot/free-vars.scm misc/scripts misc/logo boot/baselib/match.scm doc \
boot/free-vars-decl.scm \
boot/runtimes/psyntax-mosh/psyntax.scm \
boot/runtimes/psyntax-mosh/psyntax \
boot/runtimes/psyntax-mosh/Makefile \
boot/runtimes/psyntax-mosh/print-prefix.ss \
doc/mosh.1 doc/mosh_config.1 \
doc/RELNOTE.md lib \
tests src/call.inc.cpp \
src/instruction.scm src/test-data.scm src/accessors.scm src/os-constants.scm \
src/Reader.y src/NumberReader.y src/scanner.re src/NumberScanner.re \
doc README.md misc/ \
boot/runtimes/srfi-mosh/Makefile \
boot/runtimes/srfi-mosh/README \
boot/runtimes/srfi-mosh/bootstrap.common \
boot/runtimes/srfi-mosh/bootstrap.vanilla-mosh \
boot/runtimes/srfi-mosh/bootstrap.psyntax-mosh \
boot/runtimes/srfi-mosh/build-run.ss \
boot/runtimes/srfi-mosh/compat-mosh-run.scm \
boot/runtimes/srfi-mosh/expander.scm \
boot/runtimes/srfi-mosh/init.ss \
boot/runtimes/srfi-mosh/init5.scm \
boot/runtimes/srfi-mosh/layout.scm \
boot/runtimes/srfi-mosh/lib.boot \
boot/runtimes/srfi-mosh/lib.rnrs \
boot/runtimes/srfi-mosh/mosh-exceptions.scm \
boot/runtimes/srfi-mosh/mosh-utils5.scm \
boot/runtimes/srfi-mosh/runtime.scm \
gen-git-build.sh \
CMakeLists.txt cmake \
extlibs/gc-cvs/include \
src/ffitest.c \
gtest/LICENSE gtest/README \
gtest/gtest/gtest.h gtest/gtest/gtest-all.cc gtest/gtest/gtest_main.cc \
src/config_mona.h \
src/ext/cairo/Library.scm src/ext/cairo/moshcairo.c src/ext/cairo/moshcairo.h \
src/ext/curses/Library.scm src/ext/curses/mosh_curses.c \
src/ext/gobject/Library.scm src/ext/gobject/mgobj.c \
src/ext/khronos/OpenCL/Library.scm src/ext/khronos/OpenCL/mosh_opencl.c \
src/ext/sqlite3/Library.scm \
src/posix/debugee/Library.scm src/posix/environ/Library.scm \
src/posix/fd/Library.scm src/posix/poll/Library.scm \
src/posix/pthread/Library.scm src/posix/ptrace/Library.scm \
src/posix/sigchld_handler/Library.scm src/posix/socket/Library.scm \
src/posix/spawn/Library.scm src/posix/terminal/Library.scm \
src/posix/wait3/Library.scm \
src/bsd/kqueue/Library.scm src/generic/Library.scm \
src/win32/aio/Library.scm src/win32/gui/Library.scm src/win32/misc/Library.scm \
src/win32/plugins/mm/Library.scm src/win32/plugins/mm/moshmm.c \
src/win32/winmain.cpp \
src/private/config.h src/win 

GENERATED = \
src/all-tests.scm src/Scanner.cpp src/NumberScanner.cpp  \
src/cprocedures.cpp src/OSConstants.h src/Object-accessors.h \
src/labels.cpp  src/Instruction.h \
src/match.h \
src/NumberReader.tab.cpp src/NumberReader.tab.hpp \
src/Reader.tab.cpp src/Reader.tab.hpp \
src/nmosh_image.cpp

# To prevent re-generate on distributed tarballs
EXTRA_DIST += $(GENERATED)

CLEANFILES = gtest.a gtest_main.a lib/libffitest.so.1.0 src/ffitest.o
MAINTAINERCLEANFILES = $(GENERATED)

#  N.B. We don't use backend's (gensym) for following reasons.
#   (a) When we read serialize libraries, we need all symbols are interned.
#       Because symbols who have same string should be eq?, even when they are loaded from separate files.

#   (b) When we precompile system libraries, we want to control the start index of gensym.
#       Since we should avoid index overlapping between pre-compile libraries and pre-compiled psyntax.
#       So using environment variable MOSH_GENSYM_PREFIX, we control the prefix.

# all fasl should be newer than *.ss and psyntax.h binary
# lib/mosh/queue.ss.mosh-fasl: $(mosh_core_libraries) psyntax.h
# 	(find . -name "*.ss.mosh-fasl" | xargs rm) ; true
# 	(find . -name "*.sls.mosh-fasl" | xargs rm) ; true
# 	MOSH_GENSYM_PREFIX=L ./mosh scripts/pre-compile-r6rs.scm scripts/pre-compile-target.scm

# update: lib/mosh/queue.ss.mosh-fasl

doc:
	mosh misc/scripts/gen-doc-from-r6rs.scm $(R6RS_DOC_PATH)
	$(NATURALDOCS) -i . -o HTML doc/html -p doc/config -s pink -xi tools -r

.PHONY: doc test 
.PHONY: ptest ntest testB testR testL ntestB ntestR ntestL install Xtest testD

bench:
#	@GOSH@ ./misc/scripts/bench.scm > /dev/null
	LANG=C gosh ./misc/scripts/bench2.scm

include Generate.mk

#### test
check: test

#FIXME: use macro
MOSHBIN ="MOSH_LOADPATH=$(abs_top_srcdir)/lib $(abs_builddir)/mosh $(MOSH_TESTARGS)"
NMOSHBIN ="NMOSH_CACHEDIR=$(abs_builddir)/nmosh-cache MOSH_LOADPATH=$(abs_top_srcdir)/lib $(abs_builddir)/nmosh $(MOSH_TESTARGS)"

run_concurrent_tests=
run_concurrent_ntests=
if RUN_CONCURRENT_TESTS
run_concurrent_tests+=testT
run_concurrent_ntests+=ntestT
endif


test: ptest ntest
Xtest: ptest ntest testD ntestD
ptest: testB testR testL $(run_concurrent_tests)
ntest: ntestB ntestR ntestL $(run_concurrent_tests)

testB: ./mosh src/all-tests.scm # lib/libffitest.so.1.0 
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest)
#	(MOSH_TARGET=$(MOSHBIN) MOSH_BUILDDIR=$(abs_top_builddir) MOSH_SRCDIR=$(abs_top_srcdir) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-ffi)
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-mosh)

testL:
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir)/tests/srfi)
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir)/tests/irregex)
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir)/tests/yuni)

testR:
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir)/tests/r6rs-test-suite)

testT:
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-concurrent)

#check-am is Automake's internal target..
#testU: check-am

ntestB: ./nmosh src/all-tests.scm # lib/libffitest.so.1.0
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest)
#	(MOSH_TARGET=$(NMOSHBIN) MOSH_BUILDDIR=$(abs_top_builddir) MOSH_SRCDIR=$(abs_top_srcdir) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-ffi)
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-nmosh)

ntestL:
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir)/tests/srfi)
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir)/tests/irregex)
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir)/tests/yuni)

ntestR:
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir)/tests/r6rs-test-suite)

ntestT:
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-concurrent)

testD:
	(MOSH_TARGET=$(MOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-for-developer)

ntestD:
	(MOSH_TARGET=$(NMOSHBIN) $(MAKE) -C $(top_srcdir) -f tests/Makefile runtest-for-developer)

# Builds gtest.a and gtest_main.a.

GTEST_CPPFLAGS_ = -I$(GTEST_DIR)

# For simplicity and to avoid depending on Google Test's
# implementation details, the dependencies specified below are
# conservative and not optimized.  This is fine as Google Test
# compiles fast and for ordinary users its source rarely changes.
gtest-all.o : $(GTEST_DIR)/gtest/gtest-all.cc
	$(CXX) $(GTEST_CPPFLAGS_) $(CXXFLAGS) -o $@ -c $<

gtest_main.o : $(GTEST_DIR)/gtest/gtest_main.cc
	$(CXX) $(GTEST_CPPFLAGS_) $(CXXFLAGS) -o $@ -c $<

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

check_PROGRAMS=
TEST_CXXFLAGS_     = $(GLOBAL_CFLAGS) $(GLOBAL_CXXFLAGS) -I$(GTEST_DIR) -I$(GTEST_DIR)/gtest -DGTEST_HAS_CLONE=0  -DTEST_SRCDIR="\"$(top_srcdir)/\""
TEST_LDADD_        = libmosh_core_for_test.a gtest_main.a $(REGEXP_LIB) $(MOSH_LDADD_ARCH)
TEST_DEPENDENCIES_ = libmosh_core_for_test.a gtest_main.a

TESTS += test_object
check_PROGRAMS += test_object
test_object_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_object_CXXFLAGS = $(TEST_CXXFLAGS_)
test_object_SOURCES = src/ObjectTest.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_object_LDADD = $(TEST_LDADD_)

TESTS += test_vm
check_PROGRAMS += test_vm
test_vm_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_vm_CXXFLAGS = $(TEST_CXXFLAGS_)
test_vm_SOURCES = src/VMTest.cpp src/TestingVM.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_vm_LDADD = $(TEST_LDADD_)

TESTS += test_port
check_PROGRAMS += test_port
test_port_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_port_CXXFLAGS = $(TEST_CXXFLAGS_)
test_port_SOURCES = src/PortTest.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_port_LDADD = $(TEST_LDADD_)

TESTS += test_fasl
check_PROGRAMS += test_fasl
test_fasl_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_fasl_CXXFLAGS = $(TEST_CXXFLAGS_)
test_fasl_SOURCES = src/FaslTest.cpp src/TestingSignalHandler.cpp src/TestingVM.cpp src/psyntax_mosh_image.cpp
test_fasl_LDADD = $(TEST_LDADD_)

TESTS += test_ffi
check_PROGRAMS += test_ffi
test_ffi_DEPENDENCIES = $(TEST_DEPENDENCIES_) lib/libffitest.so.1.0
test_ffi_CXXFLAGS = $(TEST_CXXFLAGS_)
test_ffi_SOURCES = src/FFITest.cpp src/TestingSignalHandler.cpp src/TestingVM.cpp src/psyntax_mosh_image.cpp
test_ffi_LDADD = $(TEST_LDADD_)

TESTS += test_compat
check_PROGRAMS += test_compat
test_compat_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_compat_CXXFLAGS = $(TEST_CXXFLAGS_)
test_compat_SOURCES = src/OSCompatTest.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_compat_LDADD = $(TEST_LDADD_)

TESTS += test_getopt
check_PROGRAMS += test_getopt
test_getopt_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_getopt_CXXFLAGS = $(TEST_CXXFLAGS_)
test_getopt_SOURCES = src/getoptUTest.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_getopt_LDADD = $(TEST_LDADD_)

TESTS += test_thread
check_PROGRAMS += test_thread
test_thread_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_thread_CXXFLAGS = $(TEST_CXXFLAGS_)
test_thread_SOURCES = src/OSCompatThreadTest.cpp src/TestingSignalHandler.cpp src/psyntax_mosh_image.cpp
test_thread_LDADD = $(TEST_LDADD_)

dist-hook:
	-rm -f $(top_distdir)/doc/text/Download.txt
	-rm -f $(top_distdir)/lib/mosh/config.ss
	-rm -f $(top_distdir)/lib/mosh/mysql.ss
	-rm -f $(top_distdir)/lib/libffitest.so.1.0
	-rm -f $(top_distdir)/src/config.h

# for FFI test
lib/libffitest.so.1.0: src/ffitest.c
	$(CC) -g -std=c99 -Wall -fPIC -c $< -o src/ffitest.o
	$(CC) -g $(SHLIB_SO_LDFLAGS) -o $@ src/ffitest.o

# for flymake
check-syntax:
	$(CXX) -o nul $(GLOBAL_CFLAGS) $(GLOBAL_CXXFLAGS) $(INCLUDES) $(DEFAULT_INCLUDES) -DHAVE_CONFIG_H -S ${CHK_SOURCES}
