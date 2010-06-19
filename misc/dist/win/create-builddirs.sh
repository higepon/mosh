#!/bin/bash

# you will need "make distclean" && "rm -rf gc-7.1/libatomic_ops" && touch mosh
# on your sourcedir

CONFIG=`dirname $0`/../../../configure

#FIXME: add logging fac.
make -f `dirname $0`/run-configure.mk MOSHCONFIG=$CONFIG

#FIXME: build
