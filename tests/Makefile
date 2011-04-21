.PHONY: bogus runtest runtest-mosh runtest-nmosh runtest-for-developer
all: bogus
bogus:
	@echo "Do not use this makefile."
	@false

runtest-for-developer: 
	$(MOSH_TARGET) tests/mysql.scm
	$(MOSH_TARGET) tests/dbi.scm
	$(MOSH_TARGET) tests/http.scm
	$(MOSH_TARGET) tests/socket.scm
	$(MOSH_TARGET) tests/print.scm
	$(MOSH_TARGET) tests/memcached.scm

runtest: 
	$(MOSH_TARGET) --loadpath=".:./tests/mylib" tests/use-foo.scm # test : separated loadpath
	$(MOSH_TARGET) -5 src/all-tests.scm
	$(MOSH_TARGET) tests/misc.scm
	$(MOSH_TARGET) tests/template.scm
	$(MOSH_TARGET) tests/testing.scm
	$(MOSH_TARGET) tests/comment.scm
	$(MOSH_TARGET) tests/mosh-test.scm
	$(MOSH_TARGET) tests/io-error.scm
	$(MOSH_TARGET) tests/input-output-port.scm
	$(MOSH_TARGET) tests/input-port.scm < tests/input.txt
	$(MOSH_TARGET) tests/output-port.scm
	$(MOSH_TARGET) --loadpath="tests/mylib" tests/record.scm
	$(MOSH_TARGET) tests/condition.scm
	$(MOSH_TARGET) tests/rbtree-test.scm
	$(MOSH_TARGET) tests/exception.scm
	$(MOSH_TARGET) tests/unicode.scm
	$(MOSH_TARGET) tests/srfi8.scm
	$(MOSH_TARGET) tests/srfi19.scm
	$(MOSH_TARGET) tests/clos.scm
	$(MOSH_TARGET) tests/srfi-misc.scm
	$(MOSH_TARGET) tests/lists.scm
	$(MOSH_TARGET) tests/match.scm
	$(MOSH_TARGET) tests/srfi-39.scm
	$(MOSH_TARGET) tests/concurrent.scm
	$(MOSH_TARGET) tests/concurrent-crash.scm
	$(MOSH_TARGET) tests/number.scm
	$(MOSH_TARGET) tests/cgi.scm
	$(MOSH_TARGET) tests/shorten.scm
	! $(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/import_bad_regexp.scm 2>/dev/null
	$(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/import_good_regexp.scm
	$(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/import_both_regexp.scm
	! $(MOSH_TARGET) tests/error-with-circular.scm 2>/dev/null
	$(MOSH_TARGET) tests/process.scm
	$(MOSH_TARGET) tests/annotated-pair.scm

runtest-ffi:
	(cd $(MOSH_BUILDDIR) && $(MOSH_TARGET) $(MOSH_SRCDIR)/tests/ffi.scm)

runtest-mosh:
	$(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/shared.scm
	! $(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/shared2.scm 2>/dev/null
	$(MOSH_TARGET) --disable-acc --loadpath=tests/mylib tests/shared3.scm
	# test twice
	$(MOSH_TARGET) --loadpath=tests/mylib tests/fasl.scm
	$(MOSH_TARGET) --loadpath=tests/mylib tests/fasl.scm

runtest-nmosh:
	$(MOSH_TARGET) tests/nmosh.scm
	$(MOSH_TARGET) tests/nrepl.scm
	$(MOSH_TARGET) tests/nmosh-test-stack-trace.sps
