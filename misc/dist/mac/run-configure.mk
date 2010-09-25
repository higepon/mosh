.PHONY: d32 d64
all: d32 d64

d32:
	-rm -rf darwin32
	mkdir darwin32
	(cd darwin32 && CFLAGS="-arch i386 $(CFLAGS)" CXXFLAGS="-arch i386 $(CXXFLAGS)" ../$(MOSHCONFIG))

d64:
	-rm -rf darwin64
	mkdir darwin64
	(cd darwin64 && CFLAGS="-arch x86_64 $(CFLAGS)" CXXFLAGS="-arch x86_64 $(CXXFLAGS)" ../$(MOSHCONFIG))


