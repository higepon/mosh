TESTS = #test_compiler #test63_optimized #test_test193_modified0 #test_vm_define #test_test193_modified0 #test_test153 #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm # --features debug_log_gc

test: src/lexer.rs
	make -C scripts test
	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture
	grep "not implemented" src/procs.rs | wc -l	
	grep "Object {" src/procs.rs | wc -l

src/lexer.rs: src/lexer.re2c
	re2rust $< > $@

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

clean:
	rm data/test*.scm
	make -C data clean