SUBDIRS = $(shell ls -d */)

# This setup works well with make -j 8
all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@
.PHONY: $(SUBDIRS)

