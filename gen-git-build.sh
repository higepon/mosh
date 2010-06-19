#!/bin/sh
autoreconf -ifv && \
make -C boot bootstrap && \
make -C boot
