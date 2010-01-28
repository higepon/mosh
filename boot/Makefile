SRC_DIR=../src
SCRIPTS_DIR=../misc/scripts
CORE_LIBS =                 \
    lib/fixnum.scm     \
    lib/flonum.scm     \
    lib/bitwise.scm    \
    lib/bytevector.scm \
    lib/base.scm       \
    lib/r5rs.scm       \
    lib/hashtable.scm

all: $(SRC_DIR)/match.h $(SRC_DIR)/compiler-with-library.h

$(SRC_DIR)/match.h: match.h
	cp $< $@

$(SRC_DIR)/compiler-with-library.h: compiler-with-library.h
	cp $< $@

compiler-vm-cpp.scm: compiler.scm free-vars-decl.scm
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "mosh" > $@

compiler-vm.scm: compiler.scm free-vars-decl.scm
	gosh -A $(SCRIPTS_DIR) $(SCRIPTS_DIR)/gen-compiler.scm $< "vm?" > $@

match.h: match.scmc
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $(PWD)/$<.fasl
	mosh $(SCRIPTS_DIR)/binary2c.scm $<.fasl $@

compiler-with-library.h: compiler-with-library.scmc
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $(PWD)/$<.fasl
	mosh $(SCRIPTS_DIR)/binary2c.scm $<.fasl $@

match.scmc: match.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

compiler-with-library.scmc: compiler-with-library.scm compiler-vm.scm
	gosh vm.scm compile-file-with-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g;s/|@|/@/g" $@

compiler-with-library.scmc: compiler-with-library.scm compiler-vm.scm

compiler-with-library.scm: base-library.scm compiler-vm-cpp.scm $(CORE_LIBS)
	cat $^ > $@




clean:
	$(RM) *.scmc *.scmc.fasl

