
Building MOSH/NMOSH with CMake
==============================

CMake build provides "embeddable nmosh" build that can be embedded into
applications.

LIMITATIONS
-----------

- Performance may _NOT_ be optimal when compared with standard build.
- Not every mosh features are supported. (eg. FFI call-back on Win64)
- we recommend standard (autotools) build (ie. ./configure && make) for
  system-installed (ie. /usr/local/bin/mosh) mosh.

Supported platform
------------------

In short, it's win32 only. Other platforms are may or may not work.

- Microsoft Visual Studio 2022
- Ninja + MSVC

Other CMake generators are not tested.

Source code setup
-----------------

1. Run `gen-git-build.sh` if you have cloned from our git repository.
   (It requires `gosh` (gauche) and latest compatible `mosh` on PATH)
2. Install prerequisites with `vcpkg install libatomic-ops oniguruma gmp`

Configuration
-------------

TBD.


Build
-----

(Just same as standard CMake projects.)

Known Issues
------------

TBD.
