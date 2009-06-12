# GLOBAL CFLAGS
AM_CFLAGS   = @MOSH_OPTS@
AM_CXXFLAGS = $(CFLAGS)

# Boehm GC
#BOEHM_GC_DIR = ./gc-7.1alpha3
BOEHM_GC_DIR = ./gc-7.1
GC_LIB       = $(BOEHM_GC_DIR)/.libs/libgc.a

# Regexp library oniguruma
ONIG_DIR   = ./onig-5.7.0
REGEXP_LIB = $(ONIG_DIR)/.libs/libonig.a

# for GTEST
GTEST_DIR     = gtest-1.2.1
USER_DIR      = .
TESTS         = # used later
GTEST_HEADERS_ = $(GTEST_DIR)/include/gtest/*.h \
                $(GTEST_DIR)/include/gtest/internal/*.h
GTEST_CPPFLAGS_ = -I$(GTEST_DIR) -I$(GTEST_DIR)/include

# for FFI test
libffitest.so.1.0: ffitest.c
	$(CC) -fPIC -c $<
	$(CC) -Wl @SHLIB_SO_LDFLAGS@ -o $@ ffitest.o

SUBDIRS = $(BOEHM_GC_DIR) $(ONIG_DIR) .

if DEVELOPER
bin_PROGRAMS = mosh test_port test_fasl test_ffi test_object test_vm test_compat test_getopt test_thread
else
bin_PROGRAMS = mosh
endif


bin_SCRIPTS = mosh_config

if DEVELOPER
DEBUG_FLAGS=-DDEBUG_VERSION
endif
# mosh
# -O3 causes miss compile of NumberReader.tab.cpp. (newest gcc?)
# see http://d.hatena.ne.jp/higepon/20081114/1226645373
mosh_SOURCES  = main.cpp
mosh_OPTS     = -D MONA_SCHEME -D USE_BOEHM_GC -Wall -D MOSH_LIB_PATH="\"@MOSH_LIB_PATH@\"" -I $(includedir)
mosh_CFLAGS   = $(mosh_OPTS)  -g -D_FILE_OFFSET_BITS=64 -Wall -pipe @MOSH_OPTS@ $(DEBUG_FLAGS) -Wno-deprecated # temp -Wno-deprecated
mosh_CXXFLAGS = $(mosh_CFLAGS)
mosh_LDADD = libmosh.a $(GC_LIB) $(REGEXP_LIB) @MOSH_LDADD_ARCH@

if MINGW32
mosh_LDADD += mosh-icon.o
mosh-icon.o : win/mosh.rc logo/mosh.ico
	$(WINDRES) win/mosh.rc -o mosh-icon.o
endif 

noinst_LIBRARIES   = libmosh.a
libmosh_a_CXXFLAGS = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include
libmosh_a_SOURCES = \
HeapObject.h                                                       \
Object-inl.h                                                       \
Object-accessors.h                                                 \
Pair-inl.h                                                         \
VM-inl.h                                                           \
psyntax.h                                                          \
match.h                                                            \
compiler-with-library.h                                            \
CProcedure.h                                                       \
Instruction.h                                                      \
Callable.h                                                         \
GenericMap.h                                                       \
BinaryInputPort.h                                                  \
BinaryOutputPort.h                                                 \
scheme.h                                                           \
ByteVector.h                                                       \
HashTable.h                                                        \
Pair.h                                                             \
LineBufferedFileBinaryOutputPort.h                                 \
Stack.h                                                            \
Gloc.h                         BinaryInputOutputPort.h             \
Box.h                          Port.h                              \
BinaryPort.h                   OutputPort.h                        \
Codec.h                                                            \
Transcoder.h                   Transcoder.cpp                      \
ProcedureMacro.h                                                   \
EqvHashTable.h                 EqvHashTable.cpp                    \
Compnum.h                      Compnum.cpp                         \
Fixnum.h                       Fixnum.cpp                          \
Closure.h                      Closure.cpp                         \
UtilityProcedures.h            UtilityProcedures.cpp               \
ProcessProcedures.h            ProcessProcedures.cpp               \
Reader.h                       Reader.cpp                          \
NumberReader.h                 NumberReader.cpp                    \
scheme.h                       scheme.cpp                          \
Regexp.h                       Regexp.cpp                          \
Symbol.h                       Symbol.cpp                          \
VM.h                           VM.cpp                              \
VM-Profiler.cpp                VM-Run.cpp                          \
VM-Run.h                                                           \
Scanner.h                      Scanner.cpp                         \
NumberScanner.h                NumberScanner.cpp                   \
ByteArrayBinaryInputPort.h     ByteArrayBinaryInputPort.cpp        \
ByteArrayBinaryOutputPort.h    ByteArrayBinaryOutputPort.cpp       \
ScannerHelper.h                ScannerHelper.cpp                   \
Reader.tab.hpp                 Reader.tab.cpp                      \
NumberReader.tab.hpp           NumberReader.tab.cpp                \
FileBinaryInputPort.h          FileBinaryInputPort.cpp             \
BufferedFileBinaryInputPort.h  BufferedFileBinaryInputPort.cpp     \
FileBinaryOutputPort.h         FileBinaryOutputPort.cpp            \
BufferedFileBinaryOutputPort.h BufferedFileBinaryOutputPort.cpp    \
CustomBinaryInputPort.h        CustomBinaryInputPort.cpp           \
CustomTextualInputPort.h       CustomTextualInputPort.cpp          \
CustomTextualOutputPort.h      CustomTextualOutputPort.cpp         \
CustomTextualInputOutputPort.h CustomTextualInputOutputPort.cpp    \
CustomBinaryInputOutputPort.h  CustomBinaryInputOutputPort.cpp     \
TextualOutputPort.h            TextualOutputPort.cpp               \
TextualInputPort.cpp           TextualInputPort.h                  \
TranscodedTextualInputPort.cpp TranscodedTextualInputPort.h        \
TranscodedTextualOutputPort.cpp TranscodedTextualOutputPort.h      \
StringTextualOutputPort.h      StringTextualOutputPort.cpp         \
StringTextualInputPort.h       StringTextualInputPort.cpp          \
Codec.cpp                      Codec.h                             \
UTF8Codec.cpp                  UTF8Codec.h                         \
UTF16Codec.cpp                 UTF16Codec.h                        \
UTF32Codec.cpp                 UTF32Codec.h                        \
EqHashTable.h                  EqHashTable.cpp                     \
SString.h                      SString.cpp                         \
GenericHashTable.h             GenericHashTable.cpp                \
Object.h                       Object.cpp                          \
RecordTypeDescriptor.h         RecordTypeDescriptor.cpp            \
RecordConstructorDescriptor.h  RecordConstructorDescriptor.cpp     \
Record.h                       Record.cpp                          \
RecordProcedures.cpp           RecordProcedures.h                  \
ConditionProcedures.cpp        ConditionProcedures.h               \
CompilerProcedures.cpp         CompilerProcedures.h                \
HashTableProceduures.cpp       HashTableProceduures.h              \
RegexpProcedures.cpp           RegexpProcedures.h                  \
StringProcedures.cpp           StringProcedures.h                  \
PortProcedures.cpp             PortProcedures.h                    \
ListProcedures.cpp             ListProcedures.h                    \
ErrorProcedures.cpp            ErrorProcedures.h                   \
ArithmeticProcedures.cpp       ArithmeticProcedures.h              \
FlonumProcedures.cpp           FlonumProcedures.h                  \
BitwiseProcedures.cpp          BitwiseProcedures.h                 \
ByteVectorProcedures.cpp       ByteVectorProcedures.h              \
SocketProcedures.cpp           SocketProcedures.h                  \
Equivalent.h                   Equivalent.cpp                      \
CompoundCondition.h            CompoundCondition.cpp               \
ucs4string.h                   ucs4string.cpp                      \
Vector.h                       Vector.cpp                          \
Fasl.h                         Fasl.cpp                            \
Arithmetic.h                   Arithmetic.cpp                      \
Ratnum.h                       Ratnum.cpp                          \
Bignum.h                       Bignum.cpp                          \
Flonum.h                       Flonum.cpp                          \
CodeBuilder.h                  CodeBuilder.cpp                     \
FixnumProcedures.h             FixnumProcedures.cpp                \
Latin1Codec.h                  Latin1Codec.cpp                     \
FFI.h                          FFI.cpp                             \
FFIProcedures.h                FFIProcedures.cpp                   \
CustomBinaryOutputPort.h       CustomBinaryOutputPort.cpp          \
FileBinaryInputOutputPort.h    FileBinaryInputOutputPort.cpp       \
SocketBinaryInputOutputPort.h  SocketBinaryInputOutputPort.cpp     \
TranscodedTextualInputOutputPort.h  TranscodedTextualInputOutputPort.cpp  \
BufferedFileBinaryInputOutputPort.h  BufferedFileBinaryInputOutputPort.cpp \
TestingFileBinaryOutputPort.h TestingFileBinaryInputPort.h \
TestingVM.h OSConstants.h \
getoptU.h getoptU.cpp                                                      \
StandardOutputPort.h \
StandardErrorPort.h \
StandardInputPort.h \
BlockBufferedFileBinaryOutputPort.h \
BlockBufferedFileBinaryInputOutputPort.h \
LineBufferedFileBinaryInputOutputPort.h \
MultiVMProcedures.h MultiVMProcedures.cpp \
OSCompatSocket.h OSCompatSocket.cpp \
OSCompatThread.h OSCompatThread.cpp \
VMFactory.cpp VMFactory.h \
OSCompat.h OSCompat.cpp

mosh_core_libraries = \
lib/match.ss \
lib/lambda/wiki.ss    \
lib/mosh/queue.ss    \
lib/mosh/ffi.ss    \
lib/mosh/dbi.ss    \
lib/mosh/mysql.ss    \
lib/mosh/socket.ss    \
lib/mosh/process.ss \
lib/mosh/shell.ss \
lib/mosh/shell/repl.ss \
lib/mosh/dbd/mysql.ss  \
lib/mosh/test.ss   \
lib/mosh/pp.ss     \
lib/mosh/control.ss     \
lib/mosh/concurrent.ss     \
lib/mosh/config.ss \
lib/mosh/cgi.ss  \
lib/clos/core.ss  lib/clos/helpers.ss  lib/clos/introspection.ss  lib/clos/slot-access.ss  lib/clos/user.ss \
lib/clos/bootstrap/generic-functions.ss  lib/clos/bootstrap/standard-classes.ss \
lib/clos/private/allocation.ikarus.ss         lib/clos/private/method-cache.ikarus.ss \
lib/clos/private/allocation.ss                lib/clos/private/method-cache.ss \
lib/clos/private/compat.ss                    \
lib/clos/private/core-class-layout.ss         lib/clos/private/slow-method-cache.ss \
lib/clos/private/top-sort.ss \
lib/clos/private/fast-method-cache.ikarus.ss \
lib/clos/std-protocols/add-method.ss            lib/clos/std-protocols/initialize.ss \
lib/clos/std-protocols/allocate-instance.ss     lib/clos/std-protocols/make.ss \
lib/clos/std-protocols/class-initialization.ss  lib/clos/std-protocols/print-object.ss \
lib/clos/std-protocols/generic-invocation.ss \
lib/srfi/private/include.sls \
lib/srfi/private/include/compat.mosh.sls \
lib/srfi/private/registry.sls \
lib/srfi/private/let-opt.sls \
lib/srfi/private/let-opt.sls \
lib/srfi/private/platform-features.mosh.sls \
lib/srfi/private/OS-id-features.sls \
lib/srfi/%3a0.sls \
lib/srfi/%3a0/cond-expand.sls \
lib/srfi/%3a1.sls \
lib/srfi/%3a1/lists.sls \
lib/srfi/%3a2.sls \
lib/srfi/%3a2/and-let%2a.sls \
lib/srfi/%3a6.sls \
lib/srfi/%3a6/basic-string-ports.sls \
lib/srfi/%3a6/basic-string-ports/compat.mosh.sls \
lib/srfi/%3a8.sls \
lib/srfi/%3a8/receive.sls \
lib/srfi/%3a9.sls \
lib/srfi/%3a9/records.sls \
lib/srfi/%3a11.sls \
lib/srfi/%3a11/let-values.sls \
lib/srfi/%3a13.sls \
lib/srfi/13/srfi-13.scm \
lib/srfi/%3a13/strings.sls \
lib/srfi/%3a14.sls \
lib/srfi/%3a14/char-sets.sls \
lib/srfi/14/srfi-14.scm \
lib/srfi/%3a16.sls \
lib/srfi/%3a16/case-lambda.sls \
lib/srfi/%3a19.sls \
lib/srfi/%3a19/time.sls \
lib/srfi/%3a19/time/compat.mosh.sls \
lib/srfi/19/srfi-19.scm \
lib/srfi/%3a23.sls \
lib/srfi/%3a23/error.sls \
lib/srfi/%3a26.sls \
lib/srfi/%3a26/cut.sls \
lib/srfi/26/cut.scm \
lib/srfi/%3a27.sls \
lib/srfi/%3a27/random-bits.sls \
lib/srfi/27/random.ss \
lib/srfi/%3a31.sls \
lib/srfi/%3a31/rec.sls \
lib/srfi/%3a37.sls \
lib/srfi/%3a37/args-fold.sls \
lib/srfi/37/srfi-37-reference.scm \
lib/srfi/%3a38.sls \
lib/srfi/%3a38/with-shared-structure.mosh.sls \
lib/srfi/%3a39.sls \
lib/srfi/%3a39/parameters.mosh.sls \
lib/srfi/%3a41.sls \
lib/srfi/%3a41/streams.sls \
lib/srfi/%3a41/streams/primitive.sls \
lib/srfi/%3a41/streams/derived.sls \
lib/srfi/%3a42.sls \
lib/srfi/%3a42/eager-comprehensions.sls \
lib/srfi/42/ec.scm \
lib/srfi/%3a43.sls \
lib/srfi/%3a43/vectors.sls \
lib/srfi/43/vector-lib.scm \
lib/srfi/%3a48.sls \
lib/srfi/%3a48/intermediate-format-strings.sls \
lib/srfi/%3a48/intermediate-format-strings/compat.mosh.sls \
lib/srfi/%3a61.sls \
lib/srfi/%3a61/cond.sls \
lib/srfi/%3a64.sls \
lib/srfi/%3a64/testing.sls \
lib/srfi/64/testing.scm \
lib/srfi/%3a67.sls \
lib/srfi/%3a67/compare-procedures.sls \
lib/srfi/67/compare.ss \
lib/srfi/%3a78.sls \
lib/srfi/%3a78/lightweight-testing.sls \
lib/srfi/%3a78/lightweight-testing/compat.mosh.sls \
lib/srfi/78/check.scm \
lib/srfi/%3a98.ss \
lib/srfi/%3a98/os-environment-variables.sls\
lib/srfi/%3a99.sls \
lib/srfi/%3a99/records.sls \
lib/srfi/%3a99/records/helper.sls \
lib/srfi/%3a99/records/inspection.sls \
lib/srfi/%3a99/records/procedural.sls \
lib/srfi/%3a99/records/syntactic.sls

#mosh_core_fasl_libraries = $(mosh_core_libraries:.ss=.ss.fasl)

datadir = @MOSH_LIB_PATH@
data_DATA = all-tests.scm #repl.scm #tools/psyntax-r6rs/rev10_to_10/psyntax.scm
nobase_data_DATA = ${mosh_core_libraries} #${mosh_core_fasl_libraries}


PRECOMPILE_SRC   = compiler-with-library.scmc pmatch.scmc psyntax.scmc

INCLUDES       = -I $(BOEHM_GC_DIR)/include -I $(ONIG_DIR)
EXTRA_DIST     = instruction.scm vm.scm library.scm compiler.scm free-vars.scm scripts pmatch.scm test-data.scm doc repl.scm tools/psyntax-r6rs/rev10_to_10/psyntax.scm r6rs-examples tools/psyntax-r6rs RELNOTE r6rs-test-suite example lib cprocedures.cpp labels.cpp all-tests.scm test call.inc.cpp ${GTEST_DIR} ffitest.c bench win mosh.vcproj mosh.sln logo r6rs-test-suite README-WINDOWS.txt README.MINGW

GENERATED = \
all-tests.scm Scanner.cpp NumberScanner.cpp psyntax.cpp  compiler-gauche.scm \
DebugInstruction.h cprocedures.cpp      \
labels.cpp match.cpp compiler.cpp psyntax.scmc             \
pmatch.scmc compiler-with-library.scmc compiler-vm-cpp.scm  \
compiler-vm.scm compiler-with-library.scm Instruction.h    \
psyntax.h psyntax.fasl match.h match.fasl                  \
compiler-with-library.h compiler-with-library.fasl

CLEANFILES = gtest.a gtest_main.a libffitest.so.1.0
MAINTAINERCLEANFILES = $(GENERATED)

#  N.B. We don't use backend's (gensym) for following reasons.
#   (a) When we read serialize libraries, we need all symbols are interned.
#       Because symbols who have same string should be eq?, even when they are loaded from separate files.

#   (b) When we precompile system libraries, we want to control the start index of gensym.
#       Since we should avoid index overlapping between pre-compile libraries and pre-compiled psyntax.
#       So using environment variable MOSH_GENSYM_PREFIX, we control the prefix.

# all fasl should be newer than *.ss and psyntax.h binary
lib/mosh/queue.ss.mosh-fasl: $(mosh_core_libraries) psyntax.h
	(find . -name "*.ss.mosh-fasl" | xargs rm) ; true
	(find . -name "*.sls.mosh-fasl" | xargs rm) ; true
	MOSH_GENSYM_PREFIX=L ./mosh scripts/pre-compile-r6rs.scm scripts/pre-compile-target.scm

update: lib/mosh/queue.ss.mosh-fasl

# when you release comment out start
if DEVELOPER
Reader.tab.cpp: Reader.y
	bison -d $< -o $@

NumberReader.tab.cpp: NumberReader.y
	bison -p "number_yy" -d $< -o $@

Scanner.cpp : scanner.re
	re2c -u $< > $@ # -d is debug option

NumberScanner.cpp : NumberScanner.re
	re2c -cu $< > $@ # -d is debug option

## N.B. Do not use -g (optimization) option. -u causes YYCURSOR bug.
##      for "\" yen mark, -g causes infinite loop

r6rs-test:
	(cd r6rs-examples && ../mosh hello.ss)
	(cd r6rs-examples && ../mosh let-syntax.ss)
	(cd r6rs-examples && ../mosh env.ss)

#### Instruction.h
Instruction.h: ./instruction.scm
	@GOSH@ ./scripts/gen-insn.scm $(PWD)/$< > $(PWD)/$@

main.cpp: psyntax.h compiler-with-library.h match.h

./compiler-vm-cpp.scm: ./compiler.scm free-vars-decl.scm
	@GOSH@ ./scripts/gen-compiler.scm $(PWD)/$< "mosh" > $(PWD)/$@

compiler-with-library.scmc: ./compiler-with-library.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-with-macro $(PWD)/$< > $(PWD)/$@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" $(PWD)/$@

SUFFIXES: .scmc .h

.scmc.h:
	mosh ./scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$<.fasl
	mosh ./scripts/binary2c.scm $(PWD)/$<.fasl $(PWD)/$@
# 	(cd doc && mosh ../scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$<.fasl)
# 	(cd doc && mosh ../scripts/binary2c.scm $(PWD)/$<.fasl $(PWD)/$@)

pmatch.scmc: ./pmatch.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-without-macro $(PWD)/$< > $(PWD)/$@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $(PWD)/$@

# we need to replace #\null => #\nul and |...| => ...
# It is the difference between gauche and mosh.
psyntax.scmc: tools/psyntax-r6rs/rev10_to_10/psyntax.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-without-macro $(PWD)/$< > $(PWD)/$@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $(PWD)/$@

CORE_LIBS =                 \
    lib/core/fixnum.scm     \
    lib/core/flonum.scm     \
    lib/core/bitwise.scm    \
    lib/core/bytevector.scm \
    lib/core/base.scm       \
    lib/core/r5rs.scm       \
    lib/core/hashtable.scm


./compiler-with-library.scm: ./library.scm ./compiler-vm-cpp.scm $(CORE_LIBS)
	cat $^ > $(PWD)/$@

./free-vars-decl.scm: ./free-vars.scm
	@GOSH@ ./scripts/gen-free-vars-decl.scm $(PWD)/$< > $(PWD)/$@

./compiler-vm.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "vm?" > $(PWD)/$@

./compiler-vm-outer.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "vm-outer?" > $(PWD)/$@  || (rm -f $(PWD)/$@ && false)

./compiler-gauche.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "gauche" > $(PWD)/$@  || (rm -f $(PWD)/$@ && false)

labels.cpp: ./instruction.scm
	@GOSH@ ./scripts/gen-label.scm $< > $(PWD)/$@

DebugInstruction.h: instruction.scm
	@GOSH@ ./scripts/gen-short-insn.scm $(PWD)/$< > $(PWD)/$@

VM.cpp: cprocedures.cpp labels.cpp

Object.h: Object-accessors.h

./cprocedures.cpp: ./free-vars-decl.scm ./scripts/gen-cproc.scm
	@GOSH@ ./scripts/gen-cproc.scm > $@

all-tests.scm: ./test-data.scm
	mosh -5 ./scripts/gen-test.scm $< $@
#	echo 'run -5 scripts/gen-test.scm test-data.scm all-tests.scm ' | gdb ./mosh

Object-accessors.h: ./accessors.scm
	./mosh ./scripts/gen-accessors.scm > $@

OSConstants.h: ./os-constants.scm
	./mosh ./scripts/gen-os-constants.scm > $@

doc:
	./mosh scripts/gen-doc-from-r6rs.scm
	perl ~/NaturalDocs-1.4/NaturalDocs -i . -o HTML doc/html -p doc/config -s pink -xi tools
# 	@GOSH@ ./scripts/gen-doc.scm "manual" "doc/"
# 	@GOSH@ ./scripts/gen-doc-index.scm "top" "doc/"


# when you release comment out end
endif

.PHONY: bench doc test install

bench:
#	@GOSH@ ./scripts/bench.scm > /dev/null
#	LANG=C @GOSH@ ./scripts/bench2.scm

#### test
check: test

test: ./mosh all-tests.scm update libffitest.so.1.0
	./mosh -5 all-tests.scm
	./mosh test/testing.scm
	./mosh test/mosh-test.scm
	./mosh test/io-error.scm
	./mosh test/input-output-port.scm
	./mosh test/input-port.scm
	./mosh test/output-port.scm
	./mosh test/shell.scm
	./mosh test/record.scm
	./mosh test/condition.scm
	./mosh test/exception.scm
	./mosh test/unicode.scm
	./mosh test/srfi8.scm
	./mosh test/srfi19.scm
	./mosh test/ffi.scm
	./mosh test/mysql.scm
	./mosh test/clos.scm
	./mosh test/dbi.scm
	./mosh test/srfi-misc.scm
	./mosh test/lists.scm
	./mosh test/socket.scm
	./mosh test/match.scm
	./mosh test/print.scm
	./mosh test/concurrent.scm
	./mosh test/concurrent-crash.scm
	(cd r6rs-test-suite && $(MAKE) $@)


# Builds gtest.a and gtest_main.a.

# Usually you shouldn't tweak such internal variables, indicated by a
# trailing _.
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS_)

# For simplicity and to avoid depending on Google Test's
# implementation details, the dependencies specified below are
# conservative and not optimized.  This is fine as Google Test
# compiles fast and for ordinary users its source rarely changes.
gtest-all.o : $(GTEST_SRCS_)
	$(CXX) $(GTEST_CPPFLAGS_) $(CXXFLAGS) -c $(GTEST_DIR)/src/gtest-all.cc

gtest_main.o : $(GTEST_SRCS_)
	$(CXX) $(GTEST_CPPFLAGS_) $(CXXFLAGS) -c $(GTEST_DIR)/src/gtest_main.cc

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

check_PROGRAMS=
TEST_CXXFLAGS_     = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include -DGTEST_HAS_CLONE=0
TEST_LDADD_        = libmosh.a gtest_main.a $(GC_LIB) $(REGEXP_LIB)  @MOSH_LDADD_ARCH@
TEST_DEPENDENCIES_ = libmosh.a gtest_main.a

TESTS += test_object
check_PROGRAMS += test_object
test_object_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_object_CXXFLAGS = $(TEST_CXXFLAGS_)
test_object_SOURCES = ObjectTest.cpp TestingSignalHandler.cpp
test_object_LDADD = $(TEST_LDADD_)

TESTS += test_vm
check_PROGRAMS += test_vm
test_vm_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_vm_CXXFLAGS = $(TEST_CXXFLAGS_)
test_vm_SOURCES = VMTest.cpp TestingVM.cpp TestingSignalHandler.cpp
test_vm_LDADD = $(TEST_LDADD_)

TESTS += test_port
check_PROGRAMS += test_port
test_port_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_port_CXXFLAGS = $(TEST_CXXFLAGS_)
test_port_SOURCES = PortTest.cpp TestingSignalHandler.cpp
test_port_LDADD = $(TEST_LDADD_)

TESTS += test_fasl
check_PROGRAMS += test_fasl
test_fasl_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_fasl_CXXFLAGS = $(TEST_CXXFLAGS_)
test_fasl_SOURCES = FaslTest.cpp TestingSignalHandler.cpp TestingVM.cpp
test_fasl_LDADD = $(TEST_LDADD_)

TESTS += test_ffi
check_PROGRAMS += test_ffi
test_ffi_DEPENDENCIES = $(TEST_DEPENDENCIES_) libffitest.so.1.0
test_ffi_CXXFLAGS = $(TEST_CXXFLAGS_)
test_ffi_SOURCES = FFITest.cpp TestingSignalHandler.cpp TestingVM.cpp
test_ffi_LDADD = $(TEST_LDADD_)

TESTS += test_compat
check_PROGRAMS += test_compat
test_compat_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_compat_CXXFLAGS = $(TEST_CXXFLAGS_)
test_compat_SOURCES = OSCompatTest.cpp TestingSignalHandler.cpp
test_compat_LDADD = $(TEST_LDADD_)

TESTS += test_getopt
check_PROGRAMS += test_getopt
test_getopt_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_getopt_CXXFLAGS = $(TEST_CXXFLAGS_)
test_getopt_SOURCES = getoptUTest.cpp TestingSignalHandler.cpp
test_getopt_LDADD = $(TEST_LDADD_)

TESTS += test_thread
check_PROGRAMS += test_thread
test_thread_DEPENDENCIES = $(TEST_DEPENDENCIES_)
test_thread_CXXFLAGS = $(TEST_CXXFLAGS_)
test_thread_SOURCES = OSCompatThreadTest.cpp TestingSignalHandler.cpp
test_thread_LDADD = $(TEST_LDADD_)



install-data-hook :
	MOSH_GENSYM_PREFIX=I MOSH_LOADPATH=${DESTDIR}${datadir}/lib $(DESTDIR)/@bindir@/mosh ./scripts/pre-compile-r6rs.scm ./scripts/pre-compile-target.scm

