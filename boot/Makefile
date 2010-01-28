SRC_DIR=../src
SCRIPTS_DIR=../misc/scripts
CORE_LIBS =                 \
    lib/fixnum.scm     \
    lib/flonum.scm     \
    lib/bitwise.scm    \
    lib/bytevector.scm \
    lib/misc.scm       \
    lib/r5rs.scm       \
    lib/hashtable.scm

TARGET=$(SRC_DIR)/match.h $(SRC_DIR)/baselib.h $(SRC_DIR)/psyntax-mosh-image.h

all: $(TARGET)

$(SRC_DIR)/match.h: match.h
	cp $< $@

$(SRC_DIR)/baselib.h: baselib.h
	cp $< $@

$(SRC_DIR)/psyntax-mosh-image.h: psyntax-mosh-image.h
	cp $< $@

compiler-vm-cpp.scm: compiler.scm free-vars-decl.scm
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "mosh" > $@

compiler-vm.scm: compiler.scm free-vars-decl.scm
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "vm?" > $@

.SUFFIXES: .scmc .h
.scmc.h:
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $<.fasl
	mosh $(SCRIPTS_DIR)/binary2c.scm $<.fasl $@

psyntax-mosh-image.scmc: runtimes/psyntax-mosh/psyntax.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

match.scmc: lib/match.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

baselib.scmc: baselib.scm compiler-vm.scm
	gosh vm.scm compile-file-with-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" $@

baselib.scm: lib/base.scm compiler-vm-cpp.scm $(CORE_LIBS)
	cat $^ > $@

free-vars-decl.scm: free-vars.scm
	mosh $(SCRIPTS_DIR)/gen-free-vars-decl.scm $< > $@

clean:
	$(RM) *.scmc *.scmc.fasl match.h baselib.h baselib.scm compiler-vm-cpp.scm compiler-vm.scm free-vars-decl.scm

