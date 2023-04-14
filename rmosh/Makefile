.PHONY: r7rs-test r6rs-passed

TESTS = #test_compiler #test63_optimized #test_test193_modified0 #test_vm_define #test_test193_modified0 #test_test153 #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm # --features debug_log_gc

test: rust-test r7rs-test r6rs-test misc-test

## test-threads should be 1 because we have static mut CURRENT_VM, which could be shared.
rust-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --test-threads 1

rust-miri-test:	src/lexer_iter.rs src/number_lexer_iter.rs
	MIRIFLAGS="-Zmiri-symbolic-alignment-check -Zmiri-ignore-leaks" cargo +nightly miri test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --test-threads 1

scheme-test:
	make -C scripts test

r7rs-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo run  ./tests/r7rs/r7rs-tests.scm

r6rs-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/lists.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/reader.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/conditions.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/eval.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/contrib.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/mutable-strings.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/programs.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/control.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/records/procedural.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/enums.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/sorting.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/records/syntactic.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/exceptions.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/syntax-case.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/mutable-pairs.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/unicode.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/r5rs.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/fixnums.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/flonums.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/bitwise.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/hashtables.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/bytevectors.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/io/simple.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/base.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run-via-eval.sps --loadpath=./tests/r6rs-test-suite/
	cargo run  ./tests/r6rs-test-suite/tests/r6rs/run/io/ports.sps  --loadpath=./tests/r6rs-test-suite/

misc-test:
 	cargo run ./tests/output-port.scm
	cargo run ./tests/input-output-port.scm
	cargo run ./tests/number.scm
	cargo run ./tests/io-error.scm
	cargo run ./tests/mosh-test.scm
	cargo run ./tests/cgi.scm
	cargo run ./tests/condition.scm
	cargo run ./tests/rbtree-test.scm
	cargo run ./tests/exception.scm
	cargo run ./tests/srfi/srfi-194.scm	
	cargo run ./tests/import-srfi.scm	
	cargo run ./tests/r7rs/vector.scm
	cargo run ./tests/lists.scm
	cargo run ./tests/match.scm
	cargo run ./tests/srfi8.scm
	cargo run ./tests/srfi-39.scm
	cargo run ./tests/shorten.scm
	cargo run ./tests/r7rs/top-level.scm	
	cargo run ./tests/r7rs/import-all.scm
	cargo run ./tests/srfi/srfi-151.scm
	

# 	cargo run --loadpath=".:./tests/mylib" tests/use-foo.scm # test : separated loadpath
# 	cargo run -5 src/all-tests.scm

	cargo run ./tests/comment.scm
	cargo run ./tests/srfi19.scm
	cargo run ./tests/clos.scm
	cargo run ./tests/srfi-misc.scm	
	
#	
# 	
# 	cargo run ./tests/input-port.scm < tests/input.txt

# 	cargo run --loadpath="tests/mylib" tests/record.scm
# 	cargo run ./tests/unicode.scm
#	cargo run ./tests/misc.scm
	# cargo run ./tests/template.scm
	# cargo run ./tests/testing.scm	
# 	! cargo run --disable-acc --loadpath=tests/mylib tests/import_bad_regexp.scm 2>/dev/null
# 	cargo run --disable-acc --loadpath=tests/mylib tests/import_good_regexp.scm
# 	cargo run --disable-acc --loadpath=tests/mylib tests/import_both_regexp.scm
# 	! cargo run ./tests/error-with-circular.scm 2>/dev/null
# #	cargo run ./tests/process.scm
# 	cargo run ./tests/annotated-pair.scm
# 	cargo run ./tests/print.scm
# 	cargo run ./tests/jsonmsgpack.sps	
# 	# "invalid sytax error" should be shown with file name and line number.
# 	# This grep pattern should work with both mosh and nmosh.
# 	cargo run ./tests/invalid-syntax.scm 2>&1 >/dev/null | grep 'invalid-syntax.scm' -A10 -B10
# 	cargo run ./tests/wrong_argument.scm 2>&1 | grep 'wrong_argument.scm'
# cargo run -5 tests/internal-define.scm
# 	cargo run --disable-acc --loadpath=tests/mylib tests/shared.scm
# 	! cargo run --disable-acc --loadpath=tests/mylib tests/shared2.scm 2>/dev/null
# 	cargo run --disable-acc --loadpath=tests/mylib tests/shared3.scm
# 	# test twice
# 	cargo run --loadpath=tests/mylib tests/fasl.scm
# 	cargo run --loadpath=tests/mylib tests/fasl.scm
# 	cargo run ./tests/r7rs/r7rs-tests.scm


# 	$(MAKE) -C tests/r7rs/library-converter
# 	$(MAKE) test -C tests/mnist
# 	cargo run --loadpath=./tests tests/unicode-path.scm	

gen-tests:
	mosh scripts/test_data_convert.scm ./test-data.scm
	make -C data

gen-free-vars:
	mosh scripts/gen_free_vars_procs.scm ../boot/free-vars-decl.scm

gen-compiler:
	make -C boot
	cargo fmt

src/lexer_iter.rs: src/lexer_iter.re
	re2rust -u $< > $@

src/number_lexer_iter.rs: src/number_lexer_iter.re
	re2rust -c $< > $@

# Only for dev purpose
update-psyntax:
	 (cd ../boot/runtimes/psyntax-mosh/ && make && cd ../.. && make && cd ../rmosh/boot && make ../src/psyntax.rs)

clean:
	rm data/test*.scm
	make -C data clean