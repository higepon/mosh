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
Values.h             \
config.h             \
scheme.h             \
ByteVector.h         \
Closure.h            \
EqHashTable.h        \
Pair.h               \
Regexp.h             \
Stack.h              \
VM.h                 \
Vector.h             \
freeproc.h           \
Box.h                \
TypedVector.h        \
TypedVector.cpp      \
SString.cpp          \
ucs4string.h
mosh_OPTS     = -D MONA_SCHEME -D USE_BOEHM_GC -Wall -D MOSH_LIB_PATH="\"@MOSH_LIB_PATH@\""
mosh_CFLAGS   = -O2  $(mosh_OPTS) -g -fomit-frame-pointer
mosh_CXXFLAGS = $(mosh_CFLAGS)
mosh_LDADD    = $(GC_LIB) $(REGEXP_LIB) libcompiler.a

datadir = @MOSH_LIB_PATH@
data_DATA = all-tests.scm repl.scm

# build compiler.cpp with -O0, because -O2 is much slower.
noinst_LIBRARIES = libcompiler.a
libcompiler_a_CXXFLAGS = -fomit-frame-pointer $(mosh_OPTS)
libcompiler_a_SOURCES = compiler.cpp

# generated files should be listed here.
BUILT_SOURCES  = Instruction.h compiler.cpp labels.cpp DebugInstruction.h ./compiler-vm.scm cprocedures.cpp ./compiler-gauche.scm
INCLUDES       = -I $(BOEHM_GC_DIR)/include -I $(ONIG_DIR)
EXTRA_DIST     = instruction.scm vm.scm library.scm compiler.scm free-vars.scm scripts match.scm test-data.scm doc repl.scm

GENERATED = \
Instruction.h \
compiler.cpp \
compiler-with-library.scmc \
compiler-vm-cpp.scm \
all-tests.scm \
labels.cpp \
DebugInstruction.h \
./compiler-gauche.scm \
./compiler-vm-outer.scm \
./compiler-vm.scm
CLEANFILES = $(GENERATED) mosh-compiler.o

psyntax:
	(cd tools/psyntax-r6rs/ && rm -f psyntax.pp && cp pre-built/psyntax-petite.pp psyntax.pp && gosh gosh.r6rs.ss psyntax-buildscript.ss && gosh gosh.r6rs.ss psyntax-buildscript.ss && (cat gosh.r6rs.ss psyntax.pp > pre-built/gosh.pp) && mv psyntax.pp  pre-built/psyntax-mosh.pp)

psyntax-mosh:
	./mosh tools/psyntax-r6rs/pre-built/psyntax-mosh.pp tools/psyntax-r6rs/test.scm

psyntax-gosh:
	(cd tools/psyntax-r6rs/ && cp pre-built/psyntax-gosh.pp psyntax.pp && gosh ./gosh.r6rs.ss ./test.scm)

#compiler build without -O2
# mosh-compiler.o: compiler.cpp
# 	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(mosh_CXXFLAGS) $(CXXFLAGS) -O0 -fomit-frame-pointer -MT mosh-compiler.o -MD -MP -MF $(DEPDIR)/mosh-compiler.Tpo -c -o mosh-compiler.o `test -f 'compiler.cpp' || echo '$(srcdir)/'`compiler.cpp

#### Instruction.h
Instruction.h: ./instruction.scm
	@GOSH@ ./scripts/gen-insn.scm $(PWD)/$< > $(PWD)/$@

##### compiler generation
compiler.cpp: compiler-with-library.scmc
	@GOSH@ ./scripts/gen-pre-compiled-cpp2.scm "getCompiler" $(PWD)/$< > $(PWD)/$@
#	@GOSH@ ./scripts/gen-pre-compiled-cpp.scm "getCompiler" $(PWD)/$< > $(PWD)/$@

./compiler-vm-cpp.scm: ./compiler.scm free-vars-decl.scm
	@GOSH@ ./scripts/gen-compiler.scm $(PWD)/$< "vm-cpp?" > $(PWD)/$@

compiler-with-library.scmc: ./compiler-with-library.scm ./compiler-vm.scm
	@GOSH@ ./vm.scm compile-file $(PWD)/$< > $(PWD)/$@

./compiler-with-library.scm: ./compiler-vm-cpp.scm ./library.scm
	cat ./library.scm match.scm ./compiler-vm-cpp.scm > $(PWD)/$@

./free-vars-decl.scm: ./free-vars.scm
	@GOSH@ ./scripts/gen-free-vars-decl.scm $(PWD)/$< > $(PWD)/$@

./compiler-vm.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "vm?" > $(PWD)/$@

./compiler-vm-outer.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "vm-outer?" > $(PWD)/$@  || (rm -f $(PWD)/$@ && false)

./compiler-gauche.scm: ./compiler.scm free-vars-decl.scm
	./scripts/gen-compiler.scm $(PWD)/$< "gauche?" > $(PWD)/$@  || (rm -f $(PWD)/$@ && false)

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

all-tests.scm: ./test-data.scm
	@GOSH@ ./scripts/gen-test.scm $< > $@

doc:
	@GOSH@ ./scripts/gen-doc.scm "manual" "doc/"
	@GOSH@ ./scripts/gen-doc-index.scm "top" "doc/"

fulltest:
	./mosh -t
	./vm.scm
	./scripts/test-compiler.scm
