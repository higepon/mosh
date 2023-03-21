.PHONY: r7rs-test r6rs-passed

TESTS = #test_compiler #test63_optimized #test_test193_modified0 #test_vm_define #test_test193_modified0 #test_test153 #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm # --features debug_log_gc

test: src/lexer_iter.rs src/number_lexer_iter.rs # r7rs-test r6rs-passed
	make -C scripts test
	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture # -- --test-threads 1
	grep "not implemented" src/procs.rs | wc -l
	grep "Object {" src/procs.rs | wc -l
	# Always run tests using miri to catch errors early.
	MIRIFLAGS="-Zmiri-symbolic-alignment-check -Zmiri-ignore-leaks" cargo +nightly miri test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --

test-scheme:
	mosh --loadpath=./scripts/ tests/rust_sexp.scm

r7rs-test:
	cargo run  ../tests/r7rs/r7rs-tests.scm	

r6rs-test:
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/lists.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/reader.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/conditions.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/eval.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/contrib.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/mutable-strings.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/programs.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/control.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/records/procedural.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/enums.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/sorting.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/records/syntactic.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/exceptions.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/syntax-case.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/mutable-pairs.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/unicode.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/r5rs.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/arithmetic/fixnums.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/arithmetic/flonums.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/arithmetic/bitwise.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/hashtables.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/bytevectors.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/io/simple.sps --loadpath=../tests/r6rs-test-suite/	
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/base.sps --loadpath=../tests/r6rs-test-suite/

	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run-via-eval.sps --loadpath=../tests/r6rs-test-suite/
	cargo run  ../tests/r6rs-test-suite/tests/r6rs/run/io/ports.sps  --loadpath=../tests/r6rs-test-suite/

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