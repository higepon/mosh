.PHONY: test-load test-run
all: test-load test-run

test-load: null-irregex.ss
	$(MOSH_TARGET) null-irregex.ss

test-run: re-tests.txt test-irregex-mosh.ss
	$(MOSH_TARGET) test-irregex-mosh.ss
