TESTS = test_vm_run_add_pair  # test_vm_run_add_pair
FEATURES = --features debug_log_gc  --features debug_stress_gc


test:
	cargo test $(TESTS) $(FEATURES) -- --nocapture
	#cargo test --features debug_log_gc  -- --nocapture