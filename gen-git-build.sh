#!/bin/sh
MYMAKE=`which gmake 2>/dev/null 1>/dev/null && echo -n gmake || echo -n make`
autoreconf -ifv && \
$MYMAKE -C boot bootstrap && \
$MYMAKE -C boot && \
$MYMAKE -f Generate.mk top_srcdir=.  src/Instruction.h src/NumberReader.tab.cpp src/NumberReader.tab.hpp src/NumberScanner.cpp src/OSConstants.h src/Object-accessors.h src/Reader.tab.cpp src/Reader.tab.hpp src/Scanner.cpp src/cprocedures.cpp src/labels.cpp
