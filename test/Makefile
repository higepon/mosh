.PHONY: bogus runtest runtest-mosh runtest-nmosh
all: bogus
bogus:
	@echo "Do not use this makefile."
	@false

runtest: 
	$(MOSH_TARGET) --loadpath=".:./test/mylib" test/use-foo.scm # test : separated loadpath
	$(MOSH_TARGET) -5 src/all-tests.scm
	$(MOSH_TARGET) test/misc.scm
	$(MOSH_TARGET) test/testing.scm
	$(MOSH_TARGET) test/mosh-test.scm
	$(MOSH_TARGET) test/io-error.scm
	$(MOSH_TARGET) test/input-output-port.scm
	$(MOSH_TARGET) test/input-port.scm < test/input.txt
	$(MOSH_TARGET) test/output-port.scm
	$(MOSH_TARGET) test/record.scm
	$(MOSH_TARGET) test/condition.scm
	$(MOSH_TARGET) test/exception.scm
	$(MOSH_TARGET) test/unicode.scm
	$(MOSH_TARGET) test/srfi8.scm
	$(MOSH_TARGET) test/srfi19.scm
	$(MOSH_TARGET) test/mysql.scm
	$(MOSH_TARGET) test/clos.scm
	$(MOSH_TARGET) test/dbi.scm
	$(MOSH_TARGET) test/srfi-misc.scm
	$(MOSH_TARGET) test/lists.scm
	$(MOSH_TARGET) test/socket.scm
	$(MOSH_TARGET) test/match.scm
	$(MOSH_TARGET) test/print.scm
	$(MOSH_TARGET) test/concurrent.scm
	$(MOSH_TARGET) test/concurrent-crash.scm
	$(MOSH_TARGET) test/number.scm
	$(MOSH_TARGET) test/cgi.scm
	$(MOSH_TARGET) test/memcached.scm

runtest-ffi:
	(cd $(MOSH_BUILDDIR) && $(MOSH_TARGET) $(MOSH_SRCDIR)/test/ffi.scm)

runtest-mosh:
	$(MOSH_TARGET) test/shell.scm

runtest-nmosh:
	-$(MOSH_TARGET) test/shell.scm
	$(MOSH_TARGET) test/nmosh.scm
