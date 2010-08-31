cmake_minimum_required(VERSION 2.6)

PROJECT(mosh)
include(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
include(${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)
include(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)

set(TARGET_FLAVOR mosh)

add_definitions(-DHAVE_CONFIG_H
    -D_XOPEN_SOURCE
    -DMOSH_LIB_PATH=\"bogus\"
    -DPACKAGE=\"mosh\"
    -DPACKAGE_BUGREPORT=\"bogus\" 
    -DPACKAGE_VERSION=\"bogus\"
    -DHAVE_TR1_HASHES=1
    -DHAVE_TR1_UNORDERED_MAP=1
    -DLINE_FEED_CODE_LF=1
    -DUSE_BOEHM_GC
    -DGC_THREADS
    -D_REENTRANT
    -DLARGE_CONFIG=1
    -DTHREAD_LOCAL_ALLOC=1
    -DHAS_X86_THREAD_STATE64___RAX=1
    )

# OS Settings
if(WIN32)
    add_definitions(-DMOSH_HOST_OS=\"win32\")
elseif(APPLE)
    add_definitions(-DMOSH_HOST_OS=\"darwin\")
else(WIN32)
    add_definitions(-DMOSH_HOST_OS=\"unknown\")
endif(WIN32)
if(WIN32)
add_definitions(
    -DWINVER=0x501
    -DWIN32_LEAN_AND_MEAN
    -DGC_NOT_DLL
    -DONIG_EXTERN=extern
    -DUNICODE
    -D_UNICODE
    )
set(ARCH_INCLUDE "${PROJECT_SOURCE_DIR}/win/include")
endif(WIN32)

# includes
CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
CHECK_INCLUDE_FILE(fcntl.h HAVE_FCNTL_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILE(stdbool.h HAVE_STDBOOL_H)
CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILE(stdlib.h HAVE_STDLIB_H)
CHECK_INCLUDE_FILE(string.h HAVE_STRING_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(sys/time.h HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILE(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE(sys/wait.h HAVE_SYS_WAIT_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(utmp.h HAVE_UTMP_H)

# functions
CHECK_FUNCTION_EXISTS(alloca HAVE_ALLOCA)
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)
CHECK_FUNCTION_EXISTS(memmove HAVE_MEMMOVE)
CHECK_FUNCTION_EXISTS(memset HAVE_MEMSET)
CHECK_FUNCTION_EXISTS(strtol HAVE_STRTOL)

# sizeofs
CHECK_TYPE_SIZE(int SIZEOF_INT)
CHECK_TYPE_SIZE(short SIZEOF_SHORT)
CHECK_TYPE_SIZE(long SIZEOF_LONG)
CHECK_TYPE_SIZE(longlong SIZEOF_LONG_LONG)
CHECK_TYPE_SIZE(int64_t SIZEOF___INT64)
CHECK_TYPE_SIZE(off_t SIZEOF_OFF_T)
CHECK_TYPE_SIZE(void* SIZEOF_VOIDP)
CHECK_TYPE_SIZE(float SIZEOF_FLOAT)
CHECK_TYPE_SIZE(double SIZEOF_DOUBLE)


# GMP things
# => GMP_INCLUDE_DIR, GMP_LIBRARY

set(GMP_SEARCH true)
if(CMAKE_HOST_APPLE)
    # use GMP.framework
    set(CMAKE_FRAMEWORK_PATH .)
elseif(MSVC)
    set(GMP_DIR CACHE PATH "path to MPIR library")
    set(GMP_INCLUDE_DIR ${GMP_DIR})
    set(GMP_LIBRARY ${GMP_DIR}/mpir.lib)
    set(GMP_SEARCH false)
endif(CMAKE_HOST_APPLE)

if(GMP_SEARCH)
find_path(GMP_INCLUDE_DIR gmp.h)
find_library(GMP_LIBRARY NAMES gmp)

if (GMP_INCLUDE_DIR AND GMP_LIBRARY)
   SET(GMP_FOUND TRUE)
endif (GMP_INCLUDE_DIR AND GMP_LIBRARY)

if (GMP_FOUND)
    message(STATUS "using GMP from : ${GMP_INCLUDE_DIR}, ${GMP_LIBRARY}")
else (GMP_FOUND)
    message(FATAL_ERROR "GMP not found..")
endif (GMP_FOUND)
endif(GMP_SEARCH)


# GC things
# => gc_srcs

set(gc_core_srcs
gc-7.1/allchblk.c
gc-7.1/alloc.c
gc-7.1/blacklst.c
gc-7.1/checksums.c
gc-7.1/dbg_mlc.c
gc-7.1/dyn_load.c
gc-7.1/finalize.c
gc-7.1/gc_dlopen.c
gc-7.1/gcj_mlc.c
gc-7.1/headers.c
gc-7.1/malloc.c
gc-7.1/mallocx.c
gc-7.1/mark.c
gc-7.1/mark_rts.c
gc-7.1/misc.c
gc-7.1/new_hblk.c
gc-7.1/obj_map.c
gc-7.1/os_dep.c
gc-7.1/mach_dep.c
gc-7.1/pcr_interface.c
gc-7.1/ptr_chck.c
gc-7.1/real_malloc.c
gc-7.1/reclaim.c
gc-7.1/specific.c
gc-7.1/stubborn.c
gc-7.1/typd_mlc.c
gc-7.1/backgraph.c
gc-7.1/thread_local_alloc.c
gc-7.1/atomic_ops.c
)

set(gc_pthread_srcs
gc-7.1/pthread_support.c
gc-7.1/pthread_stop_world.c
)
set(gc_darwin_srcs
gc-7.1/darwin_stop_world.c
)
set(gc_win32_srcs
gc-7.1/win32_threads.c
)

if(WIN32)
set(gc_srcs
${gc_core_srcs}
${gc_win32_srcs}
)
elseif(APPLE)
set(gc_srcs
${gc_core_srcs}
${gc_pthread_srcs}
${gc_darwin_srcs}
)
else(WIN32)
set(gc_srcs
${gc_core_srcs}
${gc_pthread_srcs}
)
endif(WIN32)

# Onigruma things

set(onig_srcs
onig/regint.h
onig/regparse.h
onig/regenc.h
onig/st.h
onig/regerror.c
onig/regparse.c
onig/regext.c
onig/regcomp.c
onig/regexec.c
onig/reggnu.c
onig/regenc.c
onig/regsyntax.c
onig/regtrav.c
onig/regversion.c
onig/st.c
onig/regposix.c
onig/regposerr.c
onig/enc/unicode.c
onig/enc/ascii.c
onig/enc/utf8.c
onig/enc/utf16_be.c
onig/enc/utf16_le.c
onig/enc/utf32_be.c
onig/enc/utf32_le.c
onig/enc/euc_jp.c
onig/enc/sjis.c
onig/enc/iso8859_1.c
onig/enc/iso8859_2.c
onig/enc/iso8859_3.c
onig/enc/iso8859_4.c
onig/enc/iso8859_5.c
onig/enc/iso8859_6.c
onig/enc/iso8859_7.c
onig/enc/iso8859_8.c
onig/enc/iso8859_9.c
onig/enc/iso8859_10.c
onig/enc/iso8859_11.c
onig/enc/iso8859_13.c
onig/enc/iso8859_14.c
onig/enc/iso8859_15.c
onig/enc/iso8859_16.c
onig/enc/euc_tw.c
onig/enc/euc_kr.c
onig/enc/big5.c
onig/enc/gb18030.c
onig/enc/koi8_r.c
onig/enc/cp1251.c
)

# mosh

include_directories(${PROJECT_BINARY_DIR}/cmake gc-7.1/include gc-7.1/libatomic_ops-1.2/src src ${GMP_INCLUDE_DIR} onig ${ARCH_INCLUDE})


set(mosh_core_srcs
src/Transcoder.cpp
src/EqvHashTable.cpp
src/Compnum.cpp
src/Fixnum.cpp
src/Closure.cpp
src/UtilityProcedures.cpp
src/ProcessProcedures.cpp
src/Reader.cpp
src/NumberReader.cpp
src/scheme.cpp
src/Regexp.cpp
src/Symbol.cpp
src/VM.cpp
src/VM-Profiler.cpp
src/VM-Run.cpp
src/Scanner.cpp
src/NumberScanner.cpp
src/ByteArrayBinaryInputPort.cpp
src/ByteArrayBinaryOutputPort.cpp
src/ScannerHelper.cpp
src/Reader.tab.cpp
src/NumberReader.tab.cpp
src/FileBinaryInputPort.cpp
src/BufferedFileBinaryInputPort.cpp
src/FileBinaryOutputPort.cpp
src/BufferedFileBinaryOutputPort.cpp
src/CustomBinaryInputPort.cpp
src/CustomTextualInputPort.cpp
src/CustomTextualOutputPort.cpp
src/CustomTextualInputOutputPort.cpp
src/CustomBinaryInputOutputPort.cpp
src/TextualOutputPort.cpp
src/TextualInputPort.cpp
src/TranscodedTextualInputPort.cpp
src/TranscodedTextualOutputPort.cpp
src/StringTextualOutputPort.cpp
src/StringTextualInputPort.cpp
src/Codec.cpp
src/UTF8Codec.cpp
src/UTF16Codec.cpp
src/UTF32Codec.cpp
src/EqHashTable.cpp
src/SString.cpp
src/GenericHashTable.cpp
src/Object.cpp
src/CompilerProcedures.cpp
src/HashTableProceduures.cpp
src/RegexpProcedures.cpp
src/StringProcedures.cpp
src/PortProcedures.cpp
src/ListProcedures.cpp
src/ErrorProcedures.cpp
src/ArithmeticProcedures.cpp
src/FlonumProcedures.cpp
src/BitwiseProcedures.cpp
src/ByteVectorProcedures.cpp
src/SocketProcedures.cpp
src/Equivalent.cpp
src/ucs4string.cpp
src/Vector.cpp
src/FaslReader.cpp
src/FaslWriter.cpp
src/Arithmetic.cpp
src/Ratnum.cpp
src/Bignum.cpp
src/Flonum.cpp
src/CodeBuilder.cpp
src/FixnumProcedures.cpp
src/Latin1Codec.cpp
src/FFI.cpp
src/FFIProcedures.cpp
src/CustomBinaryOutputPort.cpp
src/FileBinaryInputOutputPort.cpp
src/SocketBinaryInputOutputPort.cpp
src/TranscodedTextualInputOutputPort.cpp
src/BufferedFileBinaryInputOutputPort.cpp
src/getoptU.cpp
src/MultiVMProcedures.cpp
src/OSCompatSocket.cpp
src/OSCompatThread.cpp
src/VMFactory.cpp
src/OSCompat.cpp)

if(MSVC)
set(mosh_core_srcs 
${mosh_core_srcs}
win/win.cpp)
endif(MSVC)

set(nmosh_srcs
src/main.cpp
src/nmosh_image.cpp)

set(mosh_srcs
src/main.cpp
src/psyntax_mosh_image.cpp)

configure_file(cmake/config-cmake.h.in
    ${PROJECT_BINARY_DIR}/cmake/config.h)

add_executable(${TARGET_FLAVOR} 
    ${onig_srcs} 
    ${gc_srcs} 
    ${mosh_core_srcs} 
    ${mosh_srcs})

target_link_libraries(${TARGET_FLAVOR} ${GMP_LIBRARY})

