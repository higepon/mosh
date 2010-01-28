SRC_DIR=../src
SCRIPTS_DIR=../misc/scripts

all: match.h

match.h: match.scmc
	mosh $(SCRIPTS_DIR)/scmc2fasl.scm $< $(PWD)/$<.fasl
	mosh $(SCRIPTS_DIR)/binary2c.scm $<.fasl $@
	rm $<.fasl
	rm $<

match.scmc: match.scm compiler-vm.scm
	gosh vm.scm compile-file-without-macro $< > $@
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $@

