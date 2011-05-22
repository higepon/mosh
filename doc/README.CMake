
Building MOSH/NMOSH with CMake
==============================

LIMITATIONS
-----------

- Performance may _NOT_ optimal when compared with standard build.
- Not every mosh features are supported. (eg. FFI call-back in Win64)
- we recommend standard (autotools) build (ie. ./configure && make) for
  system-installed (ie. /usr/local/mosh) mosh.
- Scheme code tests and some C++ tests are not covered by CTest.

0. Supported platform
---------------------

In short, it's win32 only. Other platforms are may or may not work.

- Microsoft Visual Studio 2010 SP1
- Apple Xcode 3.x and Mac OS X 10.6 (amd64/i386 only)
- Unix Makefile with GCC/G++ 4.3 or later
- MinGW Makefile with TDM-GCC 4.5 or later

Other CMake generators are not tested.

1. Source code setup
--------------------

- (run gen-git-build.sh if you cloned from our git repository.)

setup GMP:
- MacOS X: Copy GMP.framework to DYLD default path like ~/Library/Frameworks 
           You can build GMP.framework with misc/dist/mac/build-framework.sh
-    MSVC: Download MPIR 2.x from http://www.mpir.org and build with MSVC
           then set MOSH_GMP_DIR to this
           (c:\XXXXXX\mpir-2.x.x\build.vc10\Win32\Release)
-   MinGW: Download GMP 5.x from http://gmplib.org/ and build it.

2. Configuration
----------------

TBD.


3. Build
--------

(Just same as standard CMake projects.)

4. Known Issues
---------------

- generic: Big-endian architectures are not supported. Current CMakeList.txt
           assumes target architecture is Little-endian.
- MacOS X: FFI Callback is not supported.
-    MSVC: FFI Callback is not supported at Win64.
