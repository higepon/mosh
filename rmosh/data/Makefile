TESTS = #test_vm_alloc_many_pairs  # test_vm_run_add_pair
FEATURES = #--features debug_log_vm

SCHEME_FILES = $(wildcard *.scm)
OP_FILES = $(abspath $(patsubst %.scm,%.op,$(SCHEME_FILES)))
RUST_OP_FILES = $(abspath $(patsubst %.op,%.rsop,$(OP_FILES)))


# Generate Rmosh Ops code from tests/input/*.scm
all: $(RUST_OP_FILES)
	echo $(RUST_OP_FILES)

$(OP_FILES): %.op: %.scm
	(cd ../../boot && gosh vm.scm compile-file $< > $@)

$(RUST_OP_FILES): %.rsop: %.op ../scripts/op_convert.scm
	mosh ../scripts/op_convert.scm $< > $@

clean:
	rm $(RUST_OP_FILES) $(OP_FILES)