#!/bin/sh
autoreconf -ifv && \
./configure --enable-developer "$@" && \
make bootstrap
