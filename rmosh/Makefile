TESTS = #test_test92 #test_test62 #test_test61 #test_test19 #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm  #--features debug_log_vm --features debug_log_gc

test:
	cargo test $(TESTS) $(FEATURES) --features test_gc_size -- --nocapture

gen:
	mosh scripts/test_data_convert.scm ./test-data.scm
	make -C data

clean:
	rm data/test*.scm
	make -C data clean