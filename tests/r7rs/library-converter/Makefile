GENERATED_DIR=r6rs-generated/example
SOURCE_DIR=r7rs/example/

test: $(GENERATED_DIR)/grid.sls $(GENERATED_DIR)/life.sls
	$(MOSH_TARGET) --loadpath=r6rs-generated test.scm

$(GENERATED_DIR)/grid.sls: $(SOURCE_DIR)/grid.sls
	mkdir -p $(GENERATED_DIR)
	$(MOSH_TARGET) converter.scm $< > $@

$(GENERATED_DIR)/life.sls: $(SOURCE_DIR)/life.sls
	mkdir -p $(GENERATED_DIR)
	$(MOSH_TARGET) converter.scm $< > $@

clean:
	rm -rf $(GENERATED_DIR)