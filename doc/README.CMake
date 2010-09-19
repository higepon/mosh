
Building MOSH/NMOSH with CMake
==============================


LIMITATIONS
-----------

- This is experimental port.
- Performance may _NOT_ optimal when compared with standard build.
- Not every mosh features are supported. (eg. FFI call-back)
- we recommend standard (autotools) build (ie. ./configure && make) for
  system-installed (ie. /usr/local/mosh) mosh.

0. Supported platform
---------------------

- Apple Xcode 3.x and Mac OS X 10.6 (amd64/i386 only)
- Microsoft Visual Studio 2008, 2010 (i386 only)
- Unix Makefile with GCC/G++ 4.3 or later

other CMake generators are not tested.

1. Source code setup
--------------------

- (run gen-git-build.sh if you cloned from our git repository.)
- extract onig-5.x.x.tar.gz

setup GMP:
- MacOS X: copy GMP.framework to DYLD default path like ~/Library/Frameworks 
-    MSVC: download MPIR 2.x from http://www.mpir.org and build with MSVC
           then set MOSH_GMP_DIR to this
           (c:\XXXXXX\mpir-2.x.x\build.vc10\lib\Win32\Debug)

2. Configulation
----------------

- Specify oniguruma source tree location to MOSH_ONIG_DIR variable
- Specify GMP library location to MOSH_GMP_DIR

3. Build
--------

(same as standard CMake projects)


