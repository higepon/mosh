all: boot.scm boot5.scm nmosh_version.h

.PHONY: clean test cleancache
.SUFFIXES: .gexp .exp .ss .scm


RNRS_LIB_PREFIX = lib.rnrs/

# libraries which needed by (rnrs)
RNRS_BASE_LIBS = core/primitives \
		 core/with-syntax \
		 core/syntax-rules \
		 core/let \
		 core/derived \
		 core/identifier-syntax \
		 core/quasisyntax \
		 core/quasiquote \
		 core/let-values \
		 rnrs/base \
		 rnrs/syntax-case \
		 rnrs/control \
		 rnrs/lists \
		 rnrs/records/procedural \
		 rnrs/records/inspection \
		 rnrs/records/syntactic \
		 rnrs/conditions \
		 rnrs/io/ports \
		 rnrs/io/simple \
		 rnrs/unicode \
		 rnrs/sorting \
		 rnrs/hashtables \
		 rnrs/arithmetic/fixnums \
		 rnrs/arithmetic/flonums \
		 rnrs/arithmetic/bitwise \
		 rnrs/programs \
		 rnrs/files \
		 rnrs/exceptions \
		 rnrs/enums \
		 rnrs/bytevectors 

# libraries which are needed by expand-library.scm
RNRS_LIBS = $(RNRS_BASE_LIBS) \
	    rnrs \
	    rnrs/mutable-pairs \
	    rnrs/mutable-strings \
	    rnrs/eval \
	    rnrs/r5rs \
	    rnrs/load \
	    rnrs/eval/reflection \
	    r5rs \
	    explicit-renaming 

RNRS_FILES = $(addsuffix .ss, $(addprefix $(RNRS_LIB_PREFIX), $(RNRS_LIBS)))

# system: library loader etc.
SYS_LIB_PREFIX = lib.boot/
SYS_LIBS = mosh.generated \
	   system.generated \
	   nmosh/conditions

SYS_FILES = $(addsuffix .ss, $(addprefix $(SYS_LIB_PREFIX), $(SYS_LIBS)))

# REPL(and expensive boot-up)
REPL_LIB_PREFIX = lib.boot/
REPL_LIBS = nmosh/condition-printer.nmosh \
	    nmosh/runlib.nmosh \
            nrepl/simple \
	    nmosh/startup.nmosh

REPL_FILES = $(addsuffix .ss, $(addprefix $(REPL_LIB_PREFIX), $(REPL_LIBS)))

BOOT_FILES = compat-mosh-run.scm mosh-utils5.scm runtime.scm runtime-cache.scm r6rs.exp system.exp repl.exp mosh-exceptions.exp expander.exp init5.scm init.exp

TEMPORALS = r6rs.scm repl.scm boot.scm boot5.scm system.scm

.scm.gexp:
	gosh bootstrap.gauche/expand-library.scm $< $@

.ss.gexp:
	gosh bootstrap.gauche/expand-library.scm $< $@

.gexp.exp:
	sed -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" $< > $@

init.gexp: init.ss repl.gexp
	gosh bootstrap.gauche/expand-init.scm $< $@

r6rs.scm: $(RNRS_FILES)
	cat $(RNRS_FILES) | mosh tools/conv.ss > $@

r6rs.gexp: r6rs.scm
	gosh bootstrap.gauche/expand-r6rs.scm

repl.scm: $(REPL_FILES) system.exp
	cat $(REPL_FILES) | mosh tools/conv.ss > $@

nmosh_version.h: _timestamp git-version.mk nmosh_version_dummy.h
	cp nmosh_version_dummy.h nmosh_version.h
	-$(MAKE) -f git-version.mk
	-cp nmosh_version_git.h nmosh_version.h

expander.exp: expander.scm r6rs.exp system.exp
	gosh bootstrap.gauche/expand-base.scm
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" expander.exp

mosh-exceptions.exp: expander.scm mosh-exceptions.scm r6rs.exp system.exp
	gosh bootstrap.gauche/expand-exceptions.scm
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" mosh-exceptions.exp

system.scm: $(SYS_FILES)
	cat $(SYS_FILES) | mosh tools/conv.ss > $@

system.gexp: system.scm
	gosh bootstrap.gauche/expand-system.scm

boot.scm: $(BOOT_FILES)
	cat $(BOOT_FILES) > boot.scm
	touch _timestamp

boot5.scm: boot.scm mosh-preload5.scm
	cat mosh-preload5.scm boot.scm > boot5.scm

nmosh_image.cpp: build.ss
	nmosh build.ss
	touch _timestamp

inject-nmosh: $(RNRS_FILES) $(REPL_FILES) $(SYS_FILES) nmosh_image.cpp 
	cp nmosh_image.cpp ../../../src
	$(MAKE) nmosh_version.h #seq
	cp nmosh_version.h ../../../src

test: all 
	../../../mosh -5 boot5.scm tests/r6rs/run.sps

clean:
	rm -f nmosh_version.h nmosh_version_git.h
	rm -f *.exp
	rm -f *.exp-e
	rm -f *.gexp
	rm -f lib/*.exp
	rm -rf cache
	rm -f $(TEMPORALS)
	rm -f _timestamp
	rm -f nmosh_image.cpp

cleancache: boot.scm
	mkdir -p cache
	rm -rf cache/*
