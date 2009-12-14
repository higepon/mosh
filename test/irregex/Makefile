SRCDIR = ../..
LIBPATH = $(SRCDIR)/lib
MOSH = $(SRCDIR)/mosh 

.PHONY: test-load test-run
all: test-load test-run

test-load: null-irregex.ss
	MOSH_LOADPATH=$(LIBPATH) $(MOSH) null-irregex.ss

test-run: re-tests.txt test-irregex-mosh.ss
	MOSH_LOADPATH=$(LIBPATH) $(MOSH) test-irregex-mosh.ss


