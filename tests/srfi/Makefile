.PHONY: test-run disabled-tests

test-run:
	$(MOSH_TARGET) --loadpath=../../lib and-let.sps
	$(MOSH_TARGET) --loadpath=../../lib cut.sps
	$(MOSH_TARGET) --loadpath=../../lib version.sps

known-good:
	$(MOSH_TARGET) --loadpath=../../lib eager-comprehensions.sps

known-fail:
	$(MOSH_TARGET) --loadpath=../../lib intermediate-format-strings.sps

known-fail-psyntax-mosh:
	$(MOSH_TARGET) --loadpath=../../lib compare-procedures.sps

disabled-tests:
	$(MOSH_TARGET) --loadpath=../../lib lightweight-testing.sps
	$(MOSH_TARGET) --loadpath=../../lib print-ascii.sps
	$(MOSH_TARGET) --loadpath=../../lib random-conftest.sps
	$(MOSH_TARGET) --loadpath=../../lib random.sps
	$(MOSH_TARGET) --loadpath=../../lib rec-factorial.sps
	$(MOSH_TARGET) --loadpath=../../lib records.sps
	$(MOSH_TARGET) --loadpath=../../lib time.sps
