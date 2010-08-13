# generate source files which requires extra prerequisites
# mosh/bison/re2c
#
# this file used by both gen-git-build.sh and Makefile.am
#  1) do not use special Automake constructs (including
#   Autoconf predicates like "if DEVELOPER")
#  2) this file uses following variable
#    top_srcdir: Autoconf sets this variable to apropriate
#    directory path for the user invoked configure script.

#### Instruction.h

src/Instruction.h: src/instruction.scm
	mosh $(top_srcdir)/misc/scripts/gen-insn.scm $< $@

src/main.cpp: src/psyntax_mosh_image.cpp src/baselib.h src/match.h

src/labels.cpp: src/instruction.scm
	mosh $(top_srcdir)/misc/scripts/gen-label.scm $< $@

src/cprocedures.cpp: boot/free-vars-decl.scm 
	mosh $(top_srcdir)/misc/scripts/gen-cproc.scm $< $@

src/all-tests.scm: src/test-data.scm
	(cd $(top_srcdir) && mosh -5 misc/scripts/gen-test.scm src/test-data.scm src/all-tests.scm)
#	echo 'run -5 misc/scripts/gen-test.scm test-data.scm all-tests.scm ' | gdb ./mosh

src/Object-accessors.h: src/accessors.scm
	(cd $(top_srcdir) && mosh misc/scripts/gen-accessors.scm) > $@

src/OSConstants.h: src/os-constants.scm
	(cd $(top_srcdir) && mosh misc/scripts/gen-os-constants.scm) > $@

# READER 
src/Reader.tab.hpp: src/Reader.tab.cpp src/Reader.y

src/NumberReader.tab.hpp: src/NumberReader.tab.cpp src/NumberReader.y

src/Reader.tab.cpp: src/Reader.y
	bison -d $< -o $@

src/NumberReader.tab.cpp: src/NumberReader.y
	bison -p "number_yy" -d $< -o $@

src/Scanner.cpp : src/scanner.re
	re2c -u $< > $@ # -d is debug option

src/NumberScanner.cpp : src/NumberScanner.re
	re2c -cu $< > $@ # -d is debug option

## N.B. Do not use -g (optimization) option. -u causes YYCURSOR bug.
##      for " \ " yen mark, -g causes infinite loop
