# please check doc/README.CMake for instructions

cmake_minimum_required(VERSION 2.8)

PROJECT(mosh)
include(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
include(${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)
include(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)

ENABLE_TESTING()

# mosh/nmosh select
option(MOSH_WITH_NMOSH "build nmosh version of mosh interpreter" ON)

set(nmosh_runtime_srcs
src/nmosh_image.cpp)

set(psyntax_mosh_runtime_srcs
src/psyntax_mosh_image.cpp)

if(MOSH_WITH_NMOSH)
    set(TARGET_FLAVOR nmosh)
    add_definitions(-DWITH_NMOSH_DEFAULTS)
    set(mosh_runtime_srcs ${nmosh_runtime_srcs})
else(MOSH_WITH_NMOSH)
    set(TARGET_FLAVOR mosh)
    set(mosh_runtime_srcs ${psyntax_mosh_runtime_srcs})
endif(MOSH_WITH_NMOSH)

# external libraries
# MOSH_GMP_DIR is MSVC only
set(MOSH_ONIG_DIR CACHE PATH "path to oniguruma(http://www.geocities.jp/kosako3/oniguruma/)")

# sanity check

if(NOT EXISTS ${PROJECT_SOURCE_DIR}/${mosh_runtime_srcs})
    message(FATAL_ERROR "this ${TARET_FLAVOR} tree is not configured properly. Please install release version of mosh and run ./gen-git-build.sh")
endif()

if(NOT EXISTS ${MOSH_ONIG_DIR})
    message(SEND_ERROR "Oniguruma source-tree is missing. Please extract oniguruma(http://www.geocities.jp/kosako3/oniguruma/) as ${MOSH_ONIG_DIR} . ")
endif()


set(MOSH_VERSION "0.2.6" CACHE STRING "mosh release version string")
# set(MOSH_BUGREPORT "bogus" CACHE STRING "bugreport mail address")
set(MOSH_NAME "mosh" CACHE STRING "mosh PACKAGE_NAME")
set(MOSH_LIB_PATH "${CMAKE_INSTALL_PREFIX}/share/${MOSH_NAME}/${MOSH_VERSION}/lib")

mark_as_advanced(MOSH_VERSION MOSH_BUGREPORT MOSH_NAME MOSH_LIB_PATH)

add_definitions(-DHAVE_CONFIG_H
    -DMOSH_LIB_PATH=\"${MOSH_LIB_PATH}\"
    -DPACKAGE=\"${MOSH_NAME}\"
#   -DPACKAGE_BUGREPORT=\"${MOSH_BUGREPORT}\" 
    -DPACKAGE_VERSION=\"${MOSH_VERSION}\"
    -DUSE_BOEHM_GC
    -DGC_THREADS
    -D_REENTRANT
    -DLARGE_CONFIG=1
    -DTHREAD_LOCAL_ALLOC=1
    -DHAS_X86_THREAD_STATE64___RAX=1
    -DATOMIC_UNCOLLECTABLE=1
    -DNO_EXECUTE_PERMISSION=1
    -DALL_INTERIOR_POINTERS=1
    -DGC_GCJ_SUPPORT=1
    -DJAVA_FINALIZATION=1
    -DUSE_I686_PREFETCH
    )


# OS Settings
if(CMAKE_HOST_WIN32)
    add_definitions(-DMOSH_HOST_OS=\"win32\")
    add_definitions(-DLINE_FEED_CODE_CRLF=1)
    # we set ARCH_* here. (Win32 only. when at other O/S, these flags are
    # guessed.)
    if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86") 
        if(CMAKE_CL_64)
            # FFI not supported
        else(CMAKE_CL_64)
            add_definitions(-DARCH_IA32)
        endif(CMAKE_CL_64)
    endif()
    if(MSVC)
        add_definitions(-DHAVE_TR1_HASHES=1)
        add_definitions(-DHAVE_TR1_UNORDERED_MAP=1)
        add_definitions(-D_CRT_SECURE_NO_WARNINGS) #nasty
    else() # MayBE MinGW
        set(arch_libs ${arch_libs} shlwapi ws2_32)
        add_definitions(-static-libgcc)
        #add_definitions(-static-libstdc++)
        add_definitions(-Wno-deprecated)
        add_definitions(-fwide-exec-charset=ucs-4le)
        add_definitions(-DMOSH_MINGW32)
        add_definitions(-DHAVE_EXT_HASHES=1)
    endif(MSVC)
elseif(APPLE)
    if(XCODE_VERSION)
        add_definitions(-DHAVE_EXT_HASHES=1)
        add_definitions(-DUSE_XCODE) # to disable direct-threaded code in XCode 3.x
    endif(XCODE_VERSION)

    add_definitions(-DMOSH_HOST_OS=\"darwin\")
    add_definitions(-DLINE_FEED_CODE_LF=1)
    # do not set arch here 
else() # so it is UNIX
    add_definitions(-DHAVE_TR1_HASHES=1)
    add_definitions(-DHAVE_TR1_UNORDERED_MAP=1)
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        add_definitions(-DMOSH_HOST_OS=\"linux\")
        add_definitions(-DGTEST_HAS_PTHREAD=0) # for GC
        set(arch_libs ${arch_libs} pthread dl)
    elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
        add_definitions(-DMOSH_HOST_OS=\"bsd\")
        set(arch_libs ${arch_libs} pthread)
    else()
        add_definitions(-DMOSH_HOST_OS=\"unknown\")
    endif()
    add_definitions(-DLINE_FEED_CODE_LF=1)
endif()

if(CMAKE_HOST_WIN32)
add_definitions(
    -DWINVER=0x501
    -DGC_NOT_DLL
    -DONIG_EXTERN=extern
    )
set(ARCH_INCLUDE "${PROJECT_SOURCE_DIR}/win/include")
endif(CMAKE_HOST_WIN32)

# includes
CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
CHECK_INCLUDE_FILE(fcntl.h HAVE_FCNTL_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILE(stdbool.h HAVE_STDBOOL_H)
CHECK_INCLUDE_FILE(stdarg.h HAVE_STDARG_H)
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

if(HAVE_STDARG_H)
    add_definitions(-DHAVE_STDARG_PROTOTYPES)
endif(HAVE_STDARG_H)


# GMP things
# => GMP_INCLUDE_DIR, GMP_LIBRARY

set(GMP_SEARCH true)
if(CMAKE_HOST_APPLE)
    # use GMP.framework
    # set(CMAKE_FRAMEWORK_PATH .)
elseif(MSVC)
    set(MOSH_GMP_DIR CACHE PATH "path to MPIR library (mpir/build.vcX/[lib|dll]/Win32/[Debug|Release])")
    set(GMP_INCLUDE_DIR ${MOSH_GMP_DIR})
    set(GMP_LIBRARY ${MOSH_GMP_DIR}/mpir.lib)
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
    message(SEND_ERROR "GMP not found..")
endif (GMP_FOUND)
else (GMP_SEARCH)
# check gmp.h in GMP_INCLUDE_DIR
if(NOT EXISTS ${GMP_INCLUDE_DIR}/gmp.h)
    message(SEND_ERROR "MPIR/GMP is not installed at ${GMP_INCLUDE_DIR}.. (gmp.h could not found. please build MPIR(http://www.mpir.org) first and specify mpir/build.vc[9|10]/[lib|dll]/Win32/[Debug|Release] to MOSH_GMP_DIR.")
endif(NOT EXISTS ${GMP_INCLUDE_DIR}/gmp.h)
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
gc-7.1/libatomic_ops-1.2/src/atomic_ops.c
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
else(WIN32) # it's UNIX!!
set(gc_srcs
${gc_core_srcs}
${gc_pthread_srcs}
)
endif(WIN32)

# Onigruma things

set(onig_srcs
${MOSH_ONIG_DIR}/regint.h
${MOSH_ONIG_DIR}/regparse.h
${MOSH_ONIG_DIR}/regenc.h
${MOSH_ONIG_DIR}/st.h
${MOSH_ONIG_DIR}/regerror.c
${MOSH_ONIG_DIR}/regparse.c
${MOSH_ONIG_DIR}/regext.c
${MOSH_ONIG_DIR}/regcomp.c
${MOSH_ONIG_DIR}/regexec.c
${MOSH_ONIG_DIR}/reggnu.c
${MOSH_ONIG_DIR}/regenc.c
${MOSH_ONIG_DIR}/regsyntax.c
${MOSH_ONIG_DIR}/regtrav.c
${MOSH_ONIG_DIR}/regversion.c
${MOSH_ONIG_DIR}/st.c
${MOSH_ONIG_DIR}/regposix.c
${MOSH_ONIG_DIR}/regposerr.c
${MOSH_ONIG_DIR}/enc/unicode.c
${MOSH_ONIG_DIR}/enc/ascii.c
${MOSH_ONIG_DIR}/enc/utf8.c
${MOSH_ONIG_DIR}/enc/utf16_be.c
${MOSH_ONIG_DIR}/enc/utf16_le.c
${MOSH_ONIG_DIR}/enc/utf32_be.c
${MOSH_ONIG_DIR}/enc/utf32_le.c
${MOSH_ONIG_DIR}/enc/euc_jp.c
${MOSH_ONIG_DIR}/enc/sjis.c
${MOSH_ONIG_DIR}/enc/iso8859_1.c
${MOSH_ONIG_DIR}/enc/iso8859_2.c
${MOSH_ONIG_DIR}/enc/iso8859_3.c
${MOSH_ONIG_DIR}/enc/iso8859_4.c
${MOSH_ONIG_DIR}/enc/iso8859_5.c
${MOSH_ONIG_DIR}/enc/iso8859_6.c
${MOSH_ONIG_DIR}/enc/iso8859_7.c
${MOSH_ONIG_DIR}/enc/iso8859_8.c
${MOSH_ONIG_DIR}/enc/iso8859_9.c
${MOSH_ONIG_DIR}/enc/iso8859_10.c
${MOSH_ONIG_DIR}/enc/iso8859_11.c
${MOSH_ONIG_DIR}/enc/iso8859_13.c
${MOSH_ONIG_DIR}/enc/iso8859_14.c
${MOSH_ONIG_DIR}/enc/iso8859_15.c
${MOSH_ONIG_DIR}/enc/iso8859_16.c
${MOSH_ONIG_DIR}/enc/euc_tw.c
${MOSH_ONIG_DIR}/enc/euc_kr.c
${MOSH_ONIG_DIR}/enc/big5.c
${MOSH_ONIG_DIR}/enc/gb18030.c
${MOSH_ONIG_DIR}/enc/koi8_r.c
${MOSH_ONIG_DIR}/enc/cp1251.c
)

# mosh

include_directories(${PROJECT_BINARY_DIR}/cmake gc-7.1/include gc-7.1/libatomic_ops-1.2/src src ${GMP_INCLUDE_DIR} ${MOSH_ONIG_DIR} ${ARCH_INCLUDE} gtest)

if(CMAKE_COMPILER_IS_GNUC)
    set(ffi_stub cmake/ffi_stub.S)
else()
    message(STATUS "FFI stub was disabled (build without call-back support)")
    add_definitions(-DWITHOUT_FFI_STUB)
endif(CMAKE_COMPILER_IS_GNUC)

set(mosh_core_hdrs
src/Code.h
src/SharedReference.h
src/NonGenerativeRTDs.h
src/HeapObject.h
src/Object-inl.h
src/Object-accessors.h
src/Pair-inl.h
src/VM-inl.h
src/match.h
src/baselib.h
src/CProcedure.h
src/Instruction.h
src/Callable.h
src/GenericMap.h
src/BinaryInputPort.h
src/BinaryOutputPort.h
src/scheme.h
src/ByteVector.h
src/HashTable.h
src/Pair.h
src/ExecutableMemory.h
src/LineBufferedFileBinaryOutputPort.h
src/Stack.h
src/Gloc.h
src/BinaryInputOutputPort.h
src/Box.h
src/Port.h
src/BinaryPort.h
src/OutputPort.h
src/Codec.h
src/Transcoder.h
src/ProcedureMacro.h
src/EqvHashTable.h
src/Compnum.h
src/Fixnum.h
src/Closure.h
src/UtilityProcedures.h
src/ProcessProcedures.h
src/Reader.h
src/NumberReader.h
src/scheme.h
src/Regexp.h
src/Symbol.h
src/VM.h
src/VM-Run.h
src/Continuation.h
src/Scanner.h
src/NumberScanner.h
src/ByteArrayBinaryInputPort.h
src/ByteArrayBinaryOutputPort.h
src/ScannerHelper.h
src/FileBinaryInputPort.h
src/BufferedFileBinaryInputPort.h
src/FileBinaryOutputPort.h
src/BufferedFileBinaryOutputPort.h
src/CustomBinaryInputPort.h
src/CustomTextualInputPort.h
src/CustomTextualOutputPort.h
src/CustomTextualInputOutputPort.h
src/CustomBinaryInputOutputPort.h
src/TextualOutputPort.h
src/TextualInputPort.h
src/TranscodedTextualInputPort.h
src/TranscodedTextualOutputPort.h
src/StringTextualOutputPort.h
src/StringTextualInputPort.h
src/Codec.h
src/UTF8Codec.h
src/UTF16Codec.h
src/UTF32Codec.h
src/EqHashTable.h
src/SString.h
src/GenericHashTable.h
src/Object.h
src/CompilerProcedures.h
src/HashTableProcedures.h
src/RegexpProcedures.h
src/StringProcedures.h
src/PortProcedures.h
src/ListProcedures.h
src/ErrorProcedures.h
src/ArithmeticProcedures.h
src/FlonumProcedures.h
src/BitwiseProcedures.h
src/ByteVectorProcedures.h
src/SocketProcedures.h
src/Equivalent.h
src/ucs4string.h
src/Vector.h
src/Fasl.h
src/FaslReader.h
src/FaslWriter.h
src/Arithmetic.h
src/Ratnum.h
src/Bignum.h
src/Flonum.h
src/CodeBuilder.h
src/FixnumProcedures.h
src/Latin1Codec.h
src/FFI.h
src/FFIProcedures.h
src/CustomBinaryOutputPort.h
src/FileBinaryInputOutputPort.h
src/SocketBinaryInputOutputPort.h
src/TranscodedTextualInputOutputPort.h
src/BufferedFileBinaryInputOutputPort.h
src/TestingFileBinaryOutputPort.h
src/TestingFileBinaryInputPort.h
src/TestingVM.h
src/OSConstants.h
src/getoptU.h
src/StandardOutputPort.h
src/StandardErrorPort.h
src/StandardInputPort.h
src/BlockBufferedFileBinaryOutputPort.h
src/BlockBufferedFileBinaryInputOutputPort.h
src/LineBufferedFileBinaryInputOutputPort.h
src/MultiVMProcedures.h
src/OSCompatSocket.h
src/OSCompatThread.h
src/VMFactory.h
src/SimpleStruct.h
)

set(mosh_core_srcs
src/OSCompat.h
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
src/HashTableProcedures.cpp
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
src/OSCompat.cpp
src/stubs.cpp
${ffi_stub}
)

if(CMAKE_HOST_WIN32)
set(mosh_win32_srcs
src/win32/process.c
src/win32/process_stub.cpp)
source_group(win32 FILES ${mosh_win32_srcs})
set(mosh_core_srcs 
${mosh_core_srcs}
${mosh_win32_srcs}
win/win.cpp
win/mosh.rc)
set_source_files_properties(${mosh_core_srcs}
    PROPERTIES COMPILE_FLAGS "-D_UNICODE -DUNICODE -DWIN32_LEAN_AND_MEAN")
endif(CMAKE_HOST_WIN32)

configure_file(cmake/config-cmake.h.in
    ${PROJECT_BINARY_DIR}/cmake/config.h)

if(WIN32)
set_source_files_properties(src/main.cpp src/TestingSignalHandler.cpp
    src/TestingVM.cpp
    PROPERTIES COMPILE_FLAGS "-D_UNICODE -DUNICODE -DWIN32_LEAN_AND_MEAN")
endif(WIN32)
source_group(gc FILES ${gc_srcs})
source_group(oniguruma FILES ${onig_srcs})
add_executable(${TARGET_FLAVOR} 
    ${onig_srcs} 
    ${gc_srcs} 
    ${mosh_core_srcs} 
    ${mosh_core_hdrs}
    ${mosh_runtime_srcs}
    src/main.cpp)

target_link_libraries(${TARGET_FLAVOR} ${GMP_LIBRARY} ${arch_libs})

# tests

add_library(testinggc EXCLUDE_FROM_ALL
    ${gc_srcs})
add_library(testingmosh EXCLUDE_FROM_ALL
    ${mosh_core_srcs}
    ${PROJECT_SOURCE_DIR}/gtest/gtest/gtest-all.cc
    ${mosh_runtime_srcs} ${onig_srcs} 
    src/TestingSignalHandler.cpp
    src/TestingVM.cpp)

# GC tests
macro(add_gcguitest testname)
if(MSVC)
add_executable(gctest${testname} EXCLUDE_FROM_ALL WIN32 ${PROJECT_SOURCE_DIR}/gc-7.1/tests/${testname}.c)
else(MSVC)
add_executable(gctest${testname} EXCLUDE_FROM_ALL ${PROJECT_SOURCE_DIR}/gc-7.1/tests/${testname}.c)
endif(MSVC)
target_link_libraries(gctest${testname} testinggc ${arch_libs})
add_test(gctest-${testname} gctest${testname})
endmacro(add_gcguitest)
macro(add_gctest testname)
add_executable(gctest${testname} EXCLUDE_FROM_ALL ${PROJECT_SOURCE_DIR}/gc-7.1/tests/${testname}.c)
target_link_libraries(gctest${testname} testinggc ${arch_libs})
add_test(gctest-${testname} gctest${testname})
endmacro(add_gctest)

add_gcguitest(test) # gctest is GUI app in win32.. some historical reason
add_gctest(leak_test)
add_gctest(middle)
add_gctest(smash_test)
add_gctest(huge_test)
#add_gctest(trace_test)
if(NOT WIN32)
add_gctest(thread_leak_test)
endif(NOT WIN32)
#add_gctest(test_cpp.cc)

# mosh base tests
set(gtest_srcs ${PROJECT_SOURCE_DIR}/gtest/gtest/gtest_main.cc)

macro(add_moshcoretest testname)
if(WIN32)
set_source_files_properties(src/${testname}
    PROPERTIES COMPILE_FLAGS "-D_UNICODE -DUNICODE -DWIN32_LEAN_AND_MEAN")
endif(WIN32)
add_executable(moshtest${testname} EXCLUDE_FROM_ALL ${gtest_srcs} src/${testname}.cpp)
target_link_libraries(moshtest${testname} testingmosh testinggc ${GMP_LIBRARY} ${arch_libs})
add_test(mosh-${testname} moshtest${testname})
endmacro(add_moshcoretest)

add_moshcoretest(ObjectTest)
if(MOSH_WITH_NMOSH)
message(STATUS "VMTest is not ported to nmosh")
else(MOSH_WITH_NMOSH)
add_moshcoretest(VMTest)
endif(MOSH_WITH_NMOSH)
add_moshcoretest(PortTest)
add_moshcoretest(FaslTest)
add_moshcoretest(FFITest)
add_moshcoretest(OSCompatTest)
if(MSVC)
message(STATUS "getoptUTest is not compatible with MSVC(UTF-16) builds")
else(MSVC)
add_moshcoretest(getoptUTest)
endif(MSVC)
add_moshcoretest(OSCompatThreadTest)

file(COPY ${PROJECT_SOURCE_DIR}/lib DESTINATION ${PROJECT_BINARY_DIR})
file(COPY ${PROJECT_SOURCE_DIR}/test DESTINATION ${PROJECT_BINARY_DIR})
if(WIN32)
file(COPY ${PROJECT_SOURCE_DIR}/misc/logo/mosh.ico DESTINATION ${PROJECT_BINARY_DIR}/misc/logo)
endif(WIN32)
file(COPY ${PROJECT_SOURCE_DIR}/src/all-tests.scm DESTINATION ${PROJECT_BINARY_DIR}/src)
file(COPY ${PROJECT_SOURCE_DIR}/src/test-data.scm DESTINATION ${PROJECT_BINARY_DIR}/src)


install(TARGETS ${TARGET_FLAVOR} DESTINATION bin)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/lib 
    DESTINATION share/mosh/${MOSH_VERSION})
if(EXISTS ${MOSH_GMP_DIR}/mpir.dll)
    install(FILES ${MOSH_GMP_DIR}/mpir.dll DESTINATION bin)
endif()
