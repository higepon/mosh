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
mosh_SOURCES =      \
main.cpp             \
scheme.cpp           \
read.cpp             \
Port.cpp             \
Regexp.cpp           \
Symbol.cpp           \
VM.cpp               \
freeproc.cpp         \
CProcedure.h         \
Instruction.h        \
Port.h               \
SString.h            \
Symbol.h             \
config.h             \
scheme.h             \
ByteVector.h         \
Closure.h            \
HashTable.h          \
EqHashTable.h        \
EqvHashTable.h       \
GenericHashTable.h   \
GenericHashTable.cpp \
Pair.h               \
Regexp.h             \
Stack.h              \
VM.h                 \
Vector.h             \
freeproc.h           \
Box.h                \
TypedVector.h        \
TypedVector.cpp      \
CodeBuilder.h        \
CodeBuilder.cpp        \
RecordTypeDescriptor.h \
RecordTypeDescriptor.cpp \
RecordConstructorDescriptor.h\
RecordConstructorDescriptor.cpp\
Record.h \
Record.cpp \
RecordProcedures.cpp \
RecordProcedures.h \
ConditionProcedures.cpp \
ConditionProcedures.h \
CompilerProcedures.cpp \
CompilerProcedures.h   \
HashTableProceduures.cpp \
HashTableProceduures.h   \
SString.cpp          \
BuiltinSymbols.cpp\
CompoundCondition.h\
CompoundCondition.cpp\
Builtin.h\
ucs4string.h
mosh_OPTS     = -D MONA_SCHEME -D USE_BOEHM_GC -Wall -D MOSH_LIB_PATH="\"@MOSH_LIB_PATH@\""
#mosh_CFLAGS   = -O2  $(mosh_OPTS) -g -fomit-frame-pointer -D_FILE_OFFSET_BITS=64 -Wall
mosh_CFLAGS   = -O1  $(mosh_OPTS) -pg  -D_FILE_OFFSET_BITS=64
mosh_CXXFLAGS = $(mosh_CFLAGS)
mosh_LDADD    = $(GC_LIB) $(REGEXP_LIB) libcompiler.a libmatch.a

datadir = @MOSH_LIB_PATH@
data_DATA = all-tests.scm repl.scm r6rs-examples/psyntax.scm

# build compiler.cpp with -O0, because -O2 is much slower.
noinst_LIBRARIES = libcompiler.a libmatch.a
libcompiler_a_CXXFLAGS = -fomit-frame-pointer $(mosh_OPTS)
libcompiler_a_SOURCES = compiler.cpp

# noinst_LIBRARIES = libmatch.a
libmatch_a_CXXFLAGS = -fomit-frame-pointer $(mosh_OPTS)
libmatch_a_SOURCES = match.cpp


# generated files should be listed here.
BUILT_SOURCES  = Instruction.h compiler.cpp match.cpp BuiltinSymbols.cpp labels.cpp DebugInstruction.h ./compiler-vm.scm cprocedures.cpp ./compiler-gauche.scm
INCLUDES       = -I $(BOEHM_GC_DIR)/include -I $(ONIG_DIR)
EXTRA_DIST     = instruction.scm vm.scm library.scm compiler.scm free-vars.scm scripts match.scm test-data.scm doc repl.scm r6rs-examples/psyntax.scm r6rs-examples tools/psyntax-r6rs RELNOTE

GENERATED = \
Instruction.h \
compiler.cpp \
match.scmc \
compiler-with-library.scmc \
compiler-vm-cpp.scm \
all-tests.scm \
labels.cpp \
DebugInstruction.h \
BuiltinSymbols.cpp\
./compiler-gauche.scm \
./compiler-vm-outer.scm \
./compiler-vm.scm
CLEANFILES = $(GENERATED) mosh-compiler.o

PSYNTAX_DIR=tools/psyntax-r6rs
PSYNTAX_SRC_DIR=$(PSYNTAX_DIR)/psyntax

PSYNTAX_COMMON_SRC= \
$(PSYNTAX_SRC_DIR)/compat.ss \
$(PSYNTAX_SRC_DIR)/config.ss \
$(PSYNTAX_SRC_DIR)/internal.ss \
$(PSYNTAX_SRC_DIR)/library-manager.ss

PSYNTAX_MOSH_SRC=$(PSYNTAX_COMMON_SRC) $(PSYNTAX_SRC_DIR)/expander-mosh.ss $(PSYNTAX_SRC_DIR)/main-mosh.ss

PSYNTAX_SRC=$(PSYNTAX_COMMON_SRC) $(PSYNTAX_SRC_DIR)/expander.ss $(PSYNTAX_SRC_DIR)/main.ss

r6rs-examples/psyntax.scm: $(PSYNTAX_DIR)/psyntax-buildscript-mosh.ss $(PSYNTAX_MOSH_SRC) $(PSYNTAX_DIR)/psyntax.pp
	(cd $(PSYNTAX_DIR) && gosh gosh.r6rs.ss psyntax-buildscript-mosh.ss)
	echo "please make install ***** "

$(PSYNTAX_DIR)/psyntax.pp: $(PSYNTAX_DIR)/psyntax-buildscript.ss $(PSYNTAX_SRC)
	(cd $(PSYNTAX_DIR) && gosh gosh.r6rs.ss psyntax-buildscript.ss)

# update psyntax
# psyntax-buildscript.ss を更新する
psyntax-update:
	(cd $(PSYNTAX_DIR) && gosh gosh.r6rs.ss psyntax-buildscript.ss)


# psyntax-mosh-p:
# 	(cd tools/psyntax-r6rs/ && cp pre-built/psyntax-mosh.pp psyntax.pp && $(HOME)/mosh/mosh -p ./mosh.r6rs.ss ./hello.scm)

# psyntax-mosh:
# 	(cd tools/psyntax-r6rs/ && cp pre-built/psyntax-mosh.pp psyntax.pp && $(HOME)/mosh/mosh ./mosh.r6rs.ss ./hello.scm)

# psyntax-gosh:
# 	(cd tools/psyntax-r6rs/ && cp pre-built/psyntax-mosh.pp psyntax.pp && gosh ./gosh.r6rs.ss ./hello.scm)

r6rs-test:
	(cd r6rs-examples && ../mosh -b hello.ss)
	(cd r6rs-examples && ../mosh -b let-syntax.ss)
	(cd r6rs-examples && ../mosh -b env.ss)


#compiler build without -O2
# mosh-compiler.o: compiler.cpp
# 	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(mosh_CXXFLAGS) $(CXXFLAGS) -O0 -fomit-frame-pointer -MT mosh-compiler.o -MD -MP -MF $(DEPDIR)/mosh-compiler.Tpo -c -o mosh-compiler.o `test -f 'compiler.cpp' || echo '$(srcdir)/'`compiler.cpp

#### Instruction.h
Instruction.h: ./instruction.scm
	@GOSH@ ./scripts/gen-insn.scm $(PWD)/$< > $(PWD)/$@

##### compiler generation
BuiltinSymbols.cpp: compiler-with-library.scmc match.scmc
	@GOSH@ ./scripts/gen-pre-compiled-cpp.scm -g > $(PWD)/$@

compiler.cpp: compiler-with-library.scmc match.scmc # match.scmc is also required for collecting symbols
	@GOSH@ ./scripts/gen-pre-compiled-cpp.scm "getBuiltinCompiler" $(PWD)/$< > $(PWD)/$@

match.cpp: match.scmc compiler-with-library.scmc
	@GOSH@ ./scripts/gen-pre-compiled-cpp.scm "getBuiltinMatch" $(PWD)/$< > $(PWD)/$@

./compiler-vm-cpp.scm: ./compiler.scm free-vars-decl.scm
	@GOSH@ ./scripts/gen-compiler.scm $(PWD)/$< "mosh" > $(PWD)/$@

compiler-with-library.scmc: ./compiler-with-library.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file $(PWD)/$< > $(PWD)/$@

match.scmc: ./match.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file $(PWD)/$< > $(PWD)/$@


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

./cprocedures.cpp: ./free-vars.scm
	@GOSH@ ./scripts/gen-cproc.scm > $@

#### benchmark
.PHONY: bench doc

bench:
	@GOSH@ ./scripts/bench.scm > /dev/null

#### test
check:test

test:all-tests.scm ./mosh
	./mosh all-tests.scm
	(cd r6rs-examples && ../mosh -b record-test.ss)
	(cd r6rs-examples && ../mosh -b condition-test.ss)
	(cd r6rs-test-suite && $(MAKE) $@)

all-tests.scm: ./test-data.scm
	@GOSH@ ./scripts/gen-test.scm $< > $@

doc:
# 	@GOSH@ ./scripts/gen-doc.scm "manual" "doc/"
# 	@GOSH@ ./scripts/gen-doc-index.scm "top" "doc/"

fulltest:
	./mosh -t
	./vm.scm
	./scripts/test-compiler.scm
