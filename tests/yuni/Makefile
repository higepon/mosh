.PHONY: test-load test-run
all: test-load test-run

test-load: 
	$(MOSH_TARGET) --loadpath=../../lib null-yuni.sps

test-run:
	$(MOSH_TARGET) --loadpath=../../lib bitstream.ss
	$(MOSH_TARGET) --loadpath=../../lib packunpack-octet.ss
	$(MOSH_TARGET) --loadpath=../../lib core.ss
	$(MOSH_TARGET) --loadpath=../../lib peg.ss
	$(MOSH_TARGET) --loadpath=../../lib config.sps
