SCHEME_FILES = $(shell ls -1v *.scm)
OP_FILES = $(abspath $(patsubst %.scm,%.op,$(SCHEME_FILES)))
RUST_OP_FILES = $(abspath $(patsubst %.op,%.rsop,$(OP_FILES)))

# Generate Rmosh Ops code from tests/input/*.scm
all: $(RUST_OP_FILES)
	echo $(RUST_OP_FILES)

$(OP_FILES): %.op: %.scm
	(cd ../../boot && gosh vm.scm compile-file $< > $@)

$(RUST_OP_FILES): %.rsop: %.op # ../scripts/op_convert.scm
	mosh --loadpath=../scripts ../scripts/gen_vm_test.scm $< > $@

clean:
	rm $(RUST_OP_FILES) $(OP_FILES)
