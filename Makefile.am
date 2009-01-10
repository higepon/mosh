# GLOBAL CFLAGS
AM_CFLAGS   = -O2 -fomit-frame-pointer
AM_CXXFLAGS = $(CFLAGS)

# Boehm GC
BOEHM_GC_DIR = ./gc-7.1alpha3
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

SUBDIRS = $(BOEHM_GC_DIR) $(ONIG_DIR) .

bin_PROGRAMS = mosh test_vm test_port test_fasl

# mosh
# -O3 causes miss compile of NumberReader.tab.cpp.
# see http://d.hatena.ne.jp/higepon/20081114/1226645373
mosh_SOURCES  = main.cpp
mosh_OPTS     = -D MONA_SCHEME -D USE_BOEHM_GC -Wall -D MOSH_LIB_PATH="\"@MOSH_LIB_PATH@\""
mosh_CFLAGS   = -O3 $(mosh_OPTS)  -g -D_FILE_OFFSET_BITS=64 -Wall -momit-leaf-frame-pointer -fomit-frame-pointer #-DDEBUG_VERSION
mosh_CXXFLAGS = $(mosh_CFLAGS)
mosh_LDADD = libmosh.a $(GC_LIB) $(REGEXP_LIB)

noinst_LIBRARIES   = libmosh.a
libmosh_a_CXXFLAGS = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include
libmosh_a_SOURCES = \
HeapObject.h                                                      \
Object-inl.h                                                      \
Pair-inl.h                                                        \
VM-inl.h                                                          \
psyntax.h                                                         \
match.h                                                           \
compiler-with-library.h                                           \
read.cpp                                                          \
CProcedure.h                                                      \
Instruction.h                                                     \
Callable.h                                                        \
GenericMap.h                                                      \
BinaryInputPort.h                                                 \
BinaryOutputPort.h                                                \
config.h                                                          \
scheme.h                                                          \
ByteVector.h                                                      \
HashTable.h                                                       \
Pair.h                                                            \
Stack.h                                                           \
Box.h                                                             \
Codec.h                                                           \
Transcoder.h                  Transcoder.cpp                      \
ProcedureMacro.h                                                  \
EqvHashTable.h                EqvHashTable.cpp                    \
Compnum.h                     Compnum.cpp                         \
Fixnum.h                      Fixnum.cpp                          \
Closure.h                     Closure.cpp                         \
UtilityProcedures.h           UtilityProcedures.cpp               \
ProcessProcedures.h           ProcessProcedures.cpp               \
Reader.h                      Reader.cpp                          \
NumberReader.h                NumberReader.cpp                    \
scheme.h                      scheme.cpp                          \
Regexp.h                      Regexp.cpp                          \
Symbol.h                      Symbol.cpp                          \
VM.h                          VM.cpp                              \
VM-Profiler.cpp               VM-Run.cpp                          \
Scanner.h                     Scanner.cpp                         \
NumberScanner.h               NumberScanner.cpp                   \
ByteArrayBinaryInputPort.h    ByteArrayBinaryInputPort.cpp        \
ScannerHelper.h               ScannerHelper.cpp                   \
Reader.tab.hpp                Reader.tab.cpp                      \
NumberReader.tab.hpp          NumberReader.tab.cpp                \
FileBinaryInputPort.h         FileBinaryInputPort.cpp             \
FileBinaryOutputPort.h        FileBinaryOutputPort.cpp            \
CustomBinaryInputPort.h       CustomBinaryInputPort.cpp           \
TextualOutputPort.h           TextualOutputPort.cpp               \
TextualByteVectorOutputPort.h TextualByteVectorOutputPort.cpp     \
TextualInputPort.cpp          TextualInputPort.h                  \
StringTextualOutputPort.h     StringTextualOutputPort.cpp         \
StringTextualInputPort.h      StringTextualInputPort.cpp          \
UTF8Codec.cpp                 UTF8Codec.h                         \
UTF16Codec.cpp                UTF16Codec.h                        \
UTF32Codec.cpp                UTF32Codec.h                        \
EqHashTable.h                 EqHashTable.cpp                     \
SString.h                     SString.cpp                         \
GenericHashTable.h            GenericHashTable.cpp                \
Object.h                      Object.cpp                          \
RecordTypeDescriptor.h        RecordTypeDescriptor.cpp            \
RecordConstructorDescriptor.h RecordConstructorDescriptor.cpp     \
Record.h                      Record.cpp                          \
RecordProcedures.cpp          RecordProcedures.h                  \
ConditionProcedures.cpp       ConditionProcedures.h               \
CompilerProcedures.cpp        CompilerProcedures.h                \
HashTableProceduures.cpp      HashTableProceduures.h              \
RegexpProcedures.cpp          RegexpProcedures.h                  \
StringProcedures.cpp          StringProcedures.h                  \
PortProcedures.cpp            PortProcedures.h                    \
ListProcedures.cpp            ListProcedures.h                    \
ErrorProcedures.cpp           ErrorProcedures.h                   \
ArithmeticProcedures.cpp      ArithmeticProcedures.h              \
FlonumProcedures.cpp          FlonumProcedures.h                  \
BitwiseProcedures.cpp         BitwiseProcedures.h                 \
ByteVectorProcedures.cpp      ByteVectorProcedures.h              \
Equivalent.h                  Equivalent.cpp                      \
CompoundCondition.h           CompoundCondition.cpp               \
ucs4string.h                  ucs4string.cpp                      \
Vector.h                      Vector.cpp                          \
Fasl.h                        Fasl.cpp                            \
Arithmetic.h                  Arithmetic.cpp                      \
Ratnum.h                      Ratnum.cpp                          \
Bignum.h                      Bignum.cpp                          \
Flonum.h                      Flonum.cpp                          \
CodeBuilder.h                 CodeBuilder.cpp                     \
FinxumProcedures.h            FixnumProcedures.cpp                \
Latin1Codec.h                 Latin1Codec.cpp

datadir = @MOSH_LIB_PATH@
data_DATA = all-tests.scm repl.scm tools/psyntax-r6rs/rev10_to_10/psyntax.scm
nobase_data_DATA = lib/srfi/%3A8.ss lib/srfi/%3A98.ss lib/srfi/%3A26.ss lib/mosh/test.ss

PRECOMPILE_SRC   = compiler-with-library.scmc match.scmc psyntax.scmc

INCLUDES       = -I $(BOEHM_GC_DIR)/include -I $(ONIG_DIR)
EXTRA_DIST     = instruction.scm vm.scm library.scm compiler.scm free-vars.scm scripts match.scm test-data.scm doc repl.scm tools/psyntax-r6rs/rev10_to_10/psyntax.scm r6rs-examples tools/psyntax-r6rs RELNOTE r6rs-test-suite example lib cprocedures.cpp labels.cpp all-tests.scm test

GENERATED = \
all-tests.scm Scanner.cpp NumberScanner.cpp psyntax.cpp  compiler-gauche.scm \
DebugInstruction.h cprocedures.cpp      \
labels.cpp match.cpp compiler.cpp psyntax.scmc             \
match.scmc compiler-with-library.scmc compiler-vm-cpp.scm  \
compiler-vm.scm compiler-with-library.scm Instruction.h    \
psyntax.h psyntax.fasl match.h match.fasl                  \
compiler-with-library.h compiler-with-library.fasl

CLEANFILES = gtest.a gtest_main.a
MAINTAINERCLEANFILES = $(GENERATED)

update:
	(find . -name "*.ss.fasl" | xargs rm) ; true
	./mosh scripts/pre-compile-r6rs.scm scripts/pre-compile-target.scm

# when you release comment out start

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

SUFFIXES: .scmc .h

.scmc.h:
	mosh scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$<.fasl
	mosh scripts/binary2c.scm $(PWD)/$<.fasl $(PWD)/$@

match.scmc: ./match.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-without-macro $(PWD)/$< > $(PWD)/$@

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

./cprocedures.cpp: ./free-vars-decl.scm ./scripts/gen-cproc.scm
	@GOSH@ ./scripts/gen-cproc.scm > $@

all-tests.scm: ./test-data.scm
	mosh -5 ./scripts/gen-test.scm $< $@
#	echo 'run -5 scripts/gen-test.scm test-data.scm all-tests.scm ' | gdb ./mosh

# when you release comment out end

#### benchmark
.PHONY: bench doc test

bench:
#	@GOSH@ ./scripts/bench.scm > /dev/null
	LANG=C @GOSH@ ./scripts/bench2.scm

#### test
check: test update

test: ./mosh all-tests.scm
	./mosh -5 all-tests.scm
	./mosh r6rs-examples/record-test.ss
	./mosh r6rs-examples/condition-test.ss
	./mosh r6rs-examples/exception-test.ss
	./mosh r6rs-examples/unicode-test.ss
	./mosh r6rs-examples/shell-test.ss
	(cd r6rs-test-suite && $(MAKE) $@)
	./mosh test/srfi-19-test-suite.scm


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

check_PROGRAMS=

TESTS += test_vm
check_PROGRAMS += test_vm
test_vm_CXXFLAGS = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include -DGTEST_HAS_CLONE=0
test_vm_SOURCES = VMTest.cpp TestingVM.cpp TestingSignalHandler.cpp
test_vm_LDADD = libmosh.a gtest_main.a $(GC_LIB) $(REGEXP_LIB)

TESTS += test_port
check_PROGRAMS += test_port
test_port_CXXFLAGS = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include -DGTEST_HAS_CLONE=0
test_port_SOURCES = PortTest.cpp TestingSignalHandler.cpp
test_port_LDADD = libmosh.a gtest_main.a $(GC_LIB) $(REGEXP_LIB)

TESTS += test_fasl
check_PROGRAMS += test_fasl
test_fasl_CXXFLAGS = $(mosh_CFLAGS) $(mosh_OPTS) -I$(GTEST_DIR) -I$(GTEST_DIR)/include -DGTEST_HAS_CLONE=0
test_fasl_SOURCES = FaslTest.cpp TestingSignalHandler.cpp TestingVM.cpp
test_fasl_LDADD = libmosh.a gtest_main.a $(GC_LIB) $(REGEXP_LIB)



# doc:
# 	@GOSH@ ./scripts/gen-doc.scm "manual" "doc/"
# 	@GOSH@ ./scripts/gen-doc-index.scm "top" "doc/"

