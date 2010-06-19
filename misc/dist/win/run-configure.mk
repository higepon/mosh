.PHONY: w32
all: w32

w32:
	-rm -rf mingw32
	mkdir mingw32
	(cd mingw32 && CFLAGS="-march=i686" CXXFLAGS="-march=i686" ../$(MOSHCONFIG) --host=i386-mingw32 --disable-profiler)

