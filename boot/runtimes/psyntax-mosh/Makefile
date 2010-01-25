GIT_BRANCH_NAME=`git branch | grep '*' | cut -d ' ' -f 2`
GIT_COMMIT_HEX=`git show|grep commit`
SVN_REPOS_DIR=$(shell if [ -n "$(PSYNTAX_SVN_REPOS_DIR)" ]; then echo "$(PSYNTAX_SVN_REPOS_DIR)"; else echo "~/repos-mosh/branches"; fi)

PSYNTAX_SOURCES = psyntax-buildscript-mosh.ss \
psyntax/builders.ss \
psyntax/compat.ss \
psyntax/config.ss \
psyntax/expander.ss \
psyntax/internal.ss \
psyntax/library-manager.ss \
psyntax/main.ss

ROOT_DIR= ../../..

psyntax.h: psyntax.scmc
	mosh $(ROOT_DIR)/misc/scripts/scmc2fasl.scm $(shell pwd)/$< $(shell pwd)/$<.fasl
	mosh $(ROOT_DIR)/misc/scripts/binary2c.scm $(shell pwd)/$<.fasl $(shell pwd)/$@

psyntax.scmc: psyntax.scm $(ROOT_DIR)/boot/compiler-vm.scm
	(cd $(ROOT_DIR)/ && gosh src/vm.scm compile-file-without-macro $(shell pwd)/$< > $(shell pwd)/$@)
# we need to replace #\null => #\nul and |...| => ...
# It is the difference between gauche and mosh.
	sed -i -e "s/#\\\\null/#\\\\nul/g;s/|\.\.\.|/\.\.\./g" $(shell pwd)/$@


# 1. Expand psyntax.scm using Mosh.
# 2. svn commit to branches/psyntax.[branchname]
psyntax.scm: $(PSYNTAX_SOURCES)
	MOSH_GENSYM_PREFIX=`$(ROOT_DIR)/mosh print-prefix.ss` $(ROOT_DIR)/mosh psyntax-buildscript-mosh.ss
#	cp psyntax.scm $(SVN_REPOS_DIR)/psyntax.$(GIT_BRANCH_NAME)/
#	svn ci -m "See http://github.com/higepon/mosh $(GIT_COMMIT_HEX)" $(SVN_REPOS_DIR)/psyntax.$(GIT_BRANCH_NAME)/

inject-mosh: psyntax.h
	cp -p $< $(ROOT_DIR)

clean:
	rm -f psyntax.h psyntax.scmc *.fasl
