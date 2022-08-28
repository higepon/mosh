SUBDIRS = $(shell ls -d */)

build:
	for dir in $(SUBDIRS) ; do \
		make -C  $$dir ; \
	done
