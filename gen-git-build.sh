#!/bin/sh
mkdir -p automake
echo DIST_GIT_REVISION=\"`git describe --always`\" > automake/dist-git-revision.mk
echo DIST_GIT_COMMIT_DATE=`git log --pretty=format:"\"%cD\"" -1 HEAD` >> automake/dist-git-revision.mk
mkdir -p lib/nmosh/stubs
mkdir -p boot/r6rs/nmosh/boot
mosh --loadpath=lib misc/scripts/gen-alias-libs.sps
mosh --loadpath=lib misc/scripts/gen-nmosh-stubs.sps
mosh --loadpath=lib misc/scripts/gen-nmosh-bootstrap.sps
mosh --loadpath=lib misc/scripts/gen-corelibmk.sps
MYMAKE=`which gmake 2>/dev/null 1>/dev/null && echo gmake || echo make`
autoreconf -ifv && \
$MYMAKE -C boot/runtimes/psyntax-mosh && \
$MYMAKE -C boot && \
echo "generating free-vars-decl.scm" && \
$MYMAKE -C boot free-vars-decl.scm && \
$MYMAKE -f Generate.mk top_srcdir=.  src/Instruction.h src/NumberReader.tab.cpp src/NumberReader.tab.hpp src/NumberScanner.cpp src/OSConstants.h src/Object-accessors.h src/Reader.tab.cpp src/Reader.tab.hpp src/Scanner.cpp src/cprocedures.cpp src/labels.cpp src/all-tests.scm
chmod -w tests/read-only.txt
