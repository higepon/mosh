#!/bin/sh
MYMAKE=`which gmake 2>/dev/null 1>/dev/null && echo -n gmake || echo -n make`
echo $MYMAKE
autoreconf -ifv && \
$MYMAKE -C boot bootstrap && \
$MYMAKE -C boot
