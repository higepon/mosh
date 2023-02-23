TESTS = #test_compiler #test63_optimized #test_test193_modified0 #test_vm_define #test_test193_modified0 #test_test153 #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm # --features debug_log_gc

test: src/lexer_iter.rs src/number_lexer_iter.rs
	make -C scripts test
#	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture # -- --test-threads 1
	grep "not implemented" src/procs.rs | wc -l
	grep "Object {" src/procs.rs | wc -l
	# Always run tests using miri to catch errors early.
	MIRIFLAGS="-Zmiri-symbolic-alignment-check -Zmiri-ignore-leaks" cargo +nightly miri test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --

test-scheme:
	mosh --loadpath=./scripts/ tests/rust_sexp.scm

gen-tests:
	mosh scripts/test_data_convert.scm ./test-data.scm
	make -C data

gen-free-vars:
	mosh scripts/gen_free_vars_procs.scm ../boot/free-vars-decl.scm

gen-compiler:
	make -C boot
	cargo fmt

src/lexer_iter.rs: src/lexer_iter.re
	re2rust -c $< > $@

src/number_lexer_iter.rs: src/number_lexer_iter.re
	re2rust -c $< > $@

# Only for dev purpose
update-psyntax:
	 (cd ../boot/runtimes/psyntax-mosh/ && make && cd ../.. && make && cd ../rmosh/boot && make ../src/psyntax.rs)

clean:
	rm data/test*.scm
	make -C data clean