.PHONY: r7rs-test r6rs-passed

TESTS = #test_compiler #test63_optimized #test_test193_modified0 #test_vm_define #test_test193_modified0 #test_test153 #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm # --features debug_log_gc

test: rust-test r7rs-test r6rs-test misc-test srfi-test

## test-threads should be 1 because we have static mut CURRENT_VM, which could be shared.
rust-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --test-threads 1

rust-miri-test:	src/lexer_iter.rs src/number_lexer_iter.rs
	MIRIFLAGS="-Zmiri-symbolic-alignment-check -Zmiri-ignore-leaks" cargo +nightly miri test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture --test-threads 1

scheme-test:
	make -C scripts test

r7rs-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo run --release ./tests/r7rs/r7rs-tests.scm

r6rs-test: src/lexer_iter.rs src/number_lexer_iter.rs
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/lists.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/reader.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/conditions.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/eval.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/contrib.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/mutable-strings.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/programs.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/control.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/records/procedural.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/enums.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/sorting.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/records/syntactic.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/exceptions.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/syntax-case.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/mutable-pairs.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/unicode.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/r5rs.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/fixnums.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/flonums.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/arithmetic/bitwise.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/hashtables.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/bytevectors.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/io/simple.sps --loadpath=./tests/r6rs-test-suite/	
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/base.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run-via-eval.sps --loadpath=./tests/r6rs-test-suite/
	cargo run --release ./tests/r6rs-test-suite/tests/r6rs/run/io/ports.sps  --loadpath=./tests/r6rs-test-suite/

misc-test:
	(cd test/irregex && cargo run null-irregex-mosh.ss)
	(cd test/irregex && cargo run test-irregex-mosh.ss)
	cargo run --release ./tests/annotated-pair.scm
	cargo run --release ./tests/cgi.scm
	cargo run --release ./tests/clos.scm
	cargo run --release ./tests/comment.scm
	cargo run --release ./tests/condition.scm
	!cargo run --release ./tests/error-with-circular.scm 2>/dev/null
	cargo run --release ./tests/exception.scm
	cargo run --release ./tests/import-srfi.scm	
	cargo run --release ./tests/input-output-port.scm
	cargo run --release ./tests/input-port.scm < tests/input.txt
	cargo run --release ./tests/io-error.scm
	cargo run --release ./tests/lists.scm
	cargo run --release ./tests/match.scm
	cargo run --release ./tests/misc.scm	
	cargo run --release ./tests/mosh-test.scm
	cargo run --release ./tests/number.scm
	cargo run --release ./tests/output-port.scm
	cargo run --release ./tests/print.scm
	cargo run --release ./tests/r7rs/import-all.scm
	cargo run --release ./tests/r7rs/top-level.scm	
	cargo run --release ./tests/r7rs/vector.scm
	cargo run --release ./tests/rbtree-test.scm
	cargo run --release ./tests/regexp.scm
	cargo run --release ./tests/shorten.scm
	cargo run --release ./tests/template.scm
	cargo run --release ./tests/testing.scm
	cargo run --release ./tests/unicode.scm
	cargo run --release ./tests/wrong_argument.scm 2>&1 | grep 'wrong_argument.scm'
	cargo run tests/record.scm --loadpath="tests/mylib" 
	cargo run tests/unicode-path.scm --loadpath=./tests
	cargo run tests/use-foo.scm  --loadpath=".:./tests/mylib"

#	failing
# 	cargo run --release ./tests/jsonmsgpack.sps	
#   

# 	Not supported yet
#   cargo run --release ./tests/process.scm

srfi-test:
	cargo run --release ./tests/srfi/and-let.sps
	cargo run --release ./tests/srfi/compare-procedures.sps 
	cargo run --release ./tests/srfi/cut.sps
	cargo run --release ./tests/srfi/eager-comprehensions.sps
	cargo run --release ./tests/srfi/print-ascii.sps 
	cargo run --release ./tests/srfi/random.sps     
	cargo run --release ./tests/srfi/random.sps     
	cargo run --release ./tests/srfi/records.sps     
	cargo run --release ./tests/srfi/srfi-151.scm  
	cargo run --release ./tests/srfi/srfi-194.scm	
	cargo run --release ./tests/srfi8.scm
	cargo run --release ./tests/srfi-39.scm	
	cargo run --release ./tests/srfi-misc.scm	
	cargo run --release ./tests/srfi19.scm	
	cargo run --release ./tests/srfi/time.sps
	cargo run --release ./tests/srfi/eager-comprehensions.sps 
	# cargo run --release ./tests/srfi/intermediate-format-strings.sps 
	# cargo run --release ./tests/srfi/lightweight-testing.sps  
	# cargo run --release ./tests/srfi/version.sps      
	# killed
	# cargo run --release ./tests/srfi/random-conftest.sps 


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