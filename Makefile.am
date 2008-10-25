# GLOBAL CFLAGS
AM_CFLAGS   = -O2 -fomit-frame-pointer
AM_CXXFLAGS = $(CFLAGS)

# Boehm GC
BOEHM_GC_DIR = ./gc-7.1alpha3
GC_LIB       = $(BOEHM_GC_DIR)/.libs/libgc.a

# Regexp library oniguruma
ONIG_DIR   = ./onig-5.7.0
REGEXP_LIB = $(ONIG_DIR)/.libs/libonig.a

SUBDIRS = $(BOEHM_GC_DIR) $(ONIG_DIR) .

bin_PROGRAMS = mosh
mosh_SOURCES =                                                    \
main.cpp                                                          \
psyntax.h                                                         \
matchBinaryImage.h                                                \
compilerBinaryImage.h                                             \
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
EqvHashTable.h                                                    \
Pair.h                                                            \
Stack.h                                                           \
Box.h                                                             \
ProcedureMacro.h                                                  \
Closure.h                     Closure.cpp                         \
UtilityProcedures.h           UtilityProcedures.cpp               \
reader.h                      reader.cpp                          \
scheme.h                      scheme.cpp                          \
Regexp.h                      Regexp.cpp                          \
Symbol.h                      Symbol.cpp                          \
VM.h                          VM.cpp                              \
Scanner.h                     Scanner.cpp                         \
ByteArrayBinaryInputPort.h    ByteArrayBinaryInputPort.cpp        \
ScannerHelper.h               ScannerHelper.cpp                   \
reader.tab.hpp                reader.tab.cpp                      \
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
ByteVectorProcedures.cpp      ByteVectorProcedures.h              \
Equivalent.h                  Equivalent.cpp                      \
CompoundCondition.h           CompoundCondition.cpp               \
ucs4string.h                  ucs4string.cpp                      \
Vector.h                      Vector.cpp                          \
Fasl.h                        Fasl.cpp                            \
CodeBuilder.h                 CodeBuilder.cpp

mosh_OPTS     = -D MONA_SCHEME -D USE_BOEHM_GC -Wall -D MOSH_LIB_PATH="\"@MOSH_LIB_PATH@\""
mosh_CFLAGS   = -O2  $(mosh_OPTS) -g -fomit-frame-pointer -D_FILE_OFFSET_BITS=64 -Wall -DDEBUG_VERSION #-msse -mfpmath=sse -O3 -fstrict-aliasing -fomit-frame-pointer -momit-leaf-frame-pointer -fno-align-labels -fno-align-loops -fno-align-jumps
#mosh_CFLAGS   = -O1  $(mosh_OPTS) -pg  -D_FILE_OFFSET_BITS=64
mosh_CXXFLAGS = $(mosh_CFLAGS)
MOSHLIB_CXXFLAGS   = -fomit-frame-pointer $(mosh_OPTS) # compile libraries with -O0
datadir = @MOSH_LIB_PATH@
data_DATA = all-tests.scm repl.scm tools/psyntax-r6rs/rev10_to_10/psyntax.scm
nobase_data_DATA = lib/srfi/%3A8.ss lib/srfi/%3A98.ss lib/mosh/test.ss

#noinst_LIBRARIES = libcompiler.a libmatch.a libpsyntax.a
PRECOMPILE_SRC   = compiler-with-library.scmc match.scmc psyntax.scmc

# libcompiler_a_CXXFLAGS = $(MOSHLIB_CXXFLAGS)
# libmatch_a_CXXFLAGS    = $(MOSHLIB_CXXFLAGS)
# libpsyntax_a_CXXFLAGS  = $(MOSHLIB_CXXFLAGS)
# libcompiler_a_SOURCES  = compiler.cpp
# libmatch_a_SOURCES     = match.cpp
# libpsyntax_a_SOURCES   = psyntax.cpp

mosh_LDADD = $(GC_LIB) $(REGEXP_LIB) #$(noinst_LIBRARIES)

# # generated files should be listed here.
# BUILT_SOURCES  = Instruction.h compiler.cpp match.cpp BuiltinSymbols.cpp labels.cpp DebugInstruction.h ./compiler-vm.scm cprocedures.cpp ./compiler-gauche.scm
INCLUDES       = -I $(BOEHM_GC_DIR)/include -I $(ONIG_DIR)
EXTRA_DIST     = instruction.scm vm.scm library.scm compiler.scm free-vars.scm scripts match.scm test-data.scm doc repl.scm tools/psyntax-r6rs/rev10_to_10/psyntax.scm r6rs-examples tools/psyntax-r6rs RELNOTE r6rs-test-suite example

GENERATED = \
all-tests.scm Scanner.cpp psyntax.cpp  compiler-gauche.scm \
DebugInstruction.h cprocedures.cpp      \
labels.cpp match.cpp compiler.cpp psyntax.scmc             \
match.scmc compiler-with-library.scmc compiler-vm-cpp.scm  \
compiler-vm.scm compiler-with-library.scm Instruction.h    \
psyntax.h psyntax.fasl match.h match.fasl                  \
compiler-with-library.h compiler-with-library.fasl

CLEANFILES =
MAINTAINERCLEANFILES = $(GENERATED)

reader.tab.cpp: reader.y
	bison -d $< -o $@

Scanner.cpp : scanner.re
	/usr/local/bin/re2c -ud $< > $@ # -d is debug option
## N.B. Do not use -g (optimization) option. -u causes YYCURSOR bug.

r6rs-test:
	(cd r6rs-examples && ../mosh -b hello.ss)
	(cd r6rs-examples && ../mosh -b let-syntax.ss)
	(cd r6rs-examples && ../mosh -b env.ss)

#### Instruction.h
Instruction.h: ./instruction.scm
	@GOSH@ ./scripts/gen-insn.scm $(PWD)/$< > $(PWD)/$@

#main.cpp: psyntax.h compiler-with-library.h match.h

##### compiler generation
# BuiltinSymbols.cpp: $(PRECOMPILE_SRC) #compiler-with-library.scmc match.scmc psyntax.scmc
# 	@GOSH@ ./scripts/gen-pre-compiled-faster-cpp.scm -g > $(PWD)/$@

# compiler.cpp: compiler-with-library.scmc $(PRECOMPILE_SRC) #match.scmc  psyntax.scmc # match.scmc is also required for collecting symbols
# 	@GOSH@ ./scripts/gen-pre-compiled-faster-cpp.scm "getBuiltinCompiler" $(PWD)/$< > $(PWD)/$@

# psyntax.cpp: psyntax.scmc $(PRECOMPILE_SRC) ./scripts/gen-pre-compiled-faster-cpp.scm # match.scmc is also required for collecting symbols
# 	@GOSH@ ./scripts/gen-pre-compiled-faster-cpp.scm "getBuiltinPsyntax" $(PWD)/$< > $(PWD)/$@

# match.cpp: match.scmc $(PRECOMPILE_SRC)# compiler-with-library.scmc psyntax.scmc
# 	@GOSH@ ./scripts/gen-pre-compiled-faster-cpp.scm "getBuiltinMatch" $(PWD)/$< > $(PWD)/$@

./compiler-vm-cpp.scm: ./compiler.scm free-vars-decl.scm
	@GOSH@ ./scripts/gen-compiler.scm $(PWD)/$< "mosh" > $(PWD)/$@

compiler-with-library.scmc: ./compiler-with-library.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-with-macro $(PWD)/$< > $(PWD)/$@

# SUFFIXES: .scmc .fasl

# .scmc.fasl:
# 	./mosh -b scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$@

# SUFFIXES: .fasl .h

# .fasl.h:
# 	./mosh -b scripts/binary2c.scm $(PWD)/$< $(PWD)/$@

SUFFIXES: .scmc .h

.scmc.h:
	./mosh -b scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$<.fasl
	./mosh -b scripts/binary2c.scm $(PWD)/$<.fasl $(PWD)/$@

# match.fasl: ./match.scmc
# 	./mosh -b scripts/scmc2fasl.scm $(PWD)/$< $(PWD)/$@

# matchBinaryImage.h: ./match.fasl
# 	./mosh -b scripts/binary2c.scm $(PWD)/$< $(PWD)/$@

# compilerBinaryImage.h: ./compiler-with-library.fasl
# 	./mosh -b scripts/binary2c.scm  $(PWD)/$< $(PWD)/$@

# psyntaxBinaryImage.h: ./psyntax.fasl
# 	./mosh -b scripts/binary2c.scm $(PWD)/$< $(PWD)/$@

match.scmc: ./match.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-without-macro $(PWD)/$< > $(PWD)/$@

# we need to replace #\null => #\nul and |...| => ...
# It is the difference between gauche and mosh.
psyntax.scmc: tools/psyntax-r6rs/rev10_to_10/psyntax.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file-without-macro $(PWD)/$< > $(PWD)/$@
	sed -ie "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $(PWD)/$@

./compiler-with-library.scm: ./compiler-vm-cpp.scm ./library.scm
	cat ./library.scm ./compiler-vm-cpp.scm > $(PWD)/$@

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

./cprocedures.cpp: ./free-vars.scm ./scripts/gen-cproc.scm
	@GOSH@ ./scripts/gen-cproc.scm > $@

#### benchmark
.PHONY: bench doc

bench:
	@GOSH@ ./scripts/bench.scm > /dev/null

#### test
check:test

test:all-tests.scm ./mosh
	./mosh -r test/reader.scm
	./mosh all-tests.scm
	./mosh -b r6rs-examples/srfi8-test.ss
	./mosh -b r6rs-examples/record-test.ss
	./mosh -b r6rs-examples/condition-test.ss
	./mosh -b r6rs-examples/exception-test.ss
	./mosh -b r6rs-examples/unicode-test.ss
	(cd r6rs-test-suite && $(MAKE) $@)


all-tests.scm: ./test-data.scm
	./mosh ./scripts/gen-test.scm $< > $@

doc:
	@GOSH@ ./scripts/gen-doc.scm "manual" "doc/"
	@GOSH@ ./scripts/gen-doc-index.scm "top" "doc/"

fulltest:
	./mosh -t
	./vm.scm
	./scripts/test-compiler.scm
