.PHONY: all bootstrap always
SRC_DIR=../src
SCRIPTS_DIR=../misc/scripts
CORE_LIBS =                 \
    baselib/fixnum.scm     \
    baselib/flonum.scm     \
    baselib/bitwise.scm    \
    baselib/bytevector.scm \
    baselib/misc.scm       \
    baselib/r5rs.scm       \
    baselib/hashtable.scm

CORE_INCLUDE_LIBS = baselib/enum.scm baselib/condition.scm baselib/exception.scm baselib/records.scm

TARGET=$(SRC_DIR)/match.h $(SRC_DIR)/baselib.h $(SRC_DIR)/psyntax_mosh_image.cpp $(SRC_DIR)/nmosh_image.cpp

all: bootstrap $(TARGET)

always:
	# always target

$(SRC_DIR)/match.h: match.h
	cp $< $@

$(SRC_DIR)/baselib.h: baselib.h
	cp $< $@

$(SRC_DIR)/psyntax_mosh_image.cpp: psyntax_mosh_image.cpp
	cp $< $@

$(SRC_DIR)/nmosh_image.cpp: runtimes/srfi-mosh/nmosh_image.cpp
	cp $< $@

runtimes/srfi-mosh/nmosh_image.cpp: always
	make -C runtimes/srfi-mosh # Bootstrap using vanilla-mosh and psyntax-mosh

compiler-vm-cpp.scm: compiler.scm free-vars-decl.scm $(CORE_INCLUDE_LIBS)
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "mosh" > $@

compiler-vm.scm: compiler.scm free-vars-decl.scm $(CORE_INCLUDE_LIBS)
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "vm?" > $@

.SUFFIXES: .scmc .h
.scmc.h:
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $<.fasl
	mosh $(SCRIPTS_DIR)/binary2c.scm $<.fasl $@

psyntax_mosh_image.cpp: psyntax-mosh-image.scmc
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $<.fasl
	mosh $(SCRIPTS_DIR)/binary2cpp.scm $<.fasl $@

psyntax-mosh-image.scmc: runtimes/psyntax-mosh/psyntax.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

match.scmc: baselib/match.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

baselib.scmc: baselib.scm compiler-vm.scm
	gosh vm.scm compile-file-with-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" $@

baselib.scm: baselib/base.scm compiler-vm-cpp.scm $(CORE_LIBS)
	cat $^ > $@

free-vars-decl.scm: free-vars.scm
	mosh $(SCRIPTS_DIR)/gen-free-vars-decl.scm $< > $@

GIT_BRANCH_NAME=`git branch | grep '*' | cut -d ' ' -f 2`
SVN_REPOS="http://mosh-scheme.googlecode.com/svn/branches"

bootstrap:
	svn checkout $(SVN_REPOS)/psyntax.$(GIT_BRANCH_NAME)/ runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME) || \
    svn checkout $(SVN_REPOS)/psyntax.master/ runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME)
	cp -p runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME)/psyntax.scm runtimes/psyntax-mosh/


clean:
	$(RM) *.scmc *.scmc-e *.scmc.fasl match.h baselib.h baselib.scm compiler-vm-cpp.scm compiler-vm.scm free-vars-decl.scm *image.cpp

