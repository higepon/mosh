.PHONY: test-load test-run
all: test-load test-run

test-load: null-irregex.ss
	$(MOSH_TARGET) --loadpath=../../lib null-irregex.ss

test-run: re-tests.txt test-irregex-mosh.ss
	$(MOSH_TARGET) --loadpath=../../lib test-irregex-mosh.ss
