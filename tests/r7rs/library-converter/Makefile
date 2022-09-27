GENERATED_DIR=r6rs-generated/example
SOURCE_DIR=r7rs/example/
PSYNTAX_LIB_PATH=../../../boot/runtimes/psyntax-mosh

test: $(GENERATED_DIR)/grid.sls $(GENERATED_DIR)/life.sls
	# Use genrated R6RS library.
	$(MOSH_TARGET) --loadpath=r6rs-generated run-life.scm
	# Use R7RS library.
	$(MOSH_TARGET) --loadpath=r7rs run-life.scm	
	$(MOSH_TARGET) --loadpath=$(PSYNTAX_LIB_PATH) test.scm


$(GENERATED_DIR)/grid.sls: $(SOURCE_DIR)/grid.sls
	mkdir -p $(GENERATED_DIR)
	$(MOSH_TARGET) --loadpath=$(PSYNTAX_LIB_PATH) converter.scm $< > $@

$(GENERATED_DIR)/life.sls: $(SOURCE_DIR)/life.sls
	mkdir -p $(GENERATED_DIR)
	$(MOSH_TARGET) --loadpath=$(PSYNTAX_LIB_PATH) converter.scm  $< > $@

clean:
	rm -rf $(GENERATED_DIR)