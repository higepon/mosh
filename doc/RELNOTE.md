Mosh 0.2.8 Release Note
=======================

About This Release
------------------

Minor release to support M1 mac.

Known issues
------------

- nmosh does not support R7RS libraries

Changes
-------

- Platform:
  - macOS:
    - Added native support for M1 Mac (except FFI)

- Build system:
  - Now `configure` script relies `pkg-config` to detect and link against
    system libraries
  - CMake build now require CMake 3.0 or later

- Libraries:
  - Added SRFI 133, 158 and 194 support.
  - Added SRFI 176 support. Huge thanks to Lassi Kortela.

- Heap:
  - Updated Boehm GC to the latest.

- nmosh:
  - nmosh now uses shortened filename for cache/debug-info. The shortened
    filename will be prefixed with "@"(at).

- TinyCLOS:
  - A bool value next-method? available within method definition. See
    Issue 216 for details.

- Added `--optimize-level` commandline option.

- R7RS (Experimental)
  - Support R7RS small + (scheme vector) library.
  - Mosh passes 1200/1210 tests in r7rs-tests.scm from Chibi Scheme.
  - To demonstrate R7RS comformance, we write MNIST demo in R7RS. It works both with Gauche and Mosh.
  - Mosh can all [ecraven/r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks/pull/64).
  -

- Fixnum
  - Now Mosh has 62 bits size Fixnum in 64bits architectures.

- f64array
  - To support faster matrix operations. We introduced f64array which is 2D matrix of double.

Bug fixes
---------

- Build system:
  - (mosh mysql) and (mosh config) was not installed properly.

- Reader:
  - Fixed Issue 221 reported and patched by David Banks:
    - "."(period) was not treated as a delimiter. For example, "#t.#f"
      read as #t.

- VM:
  - VM Compiler error "[internal error] iform-count-size-upto: unknown
    iform tag:13" was fixed.

- Core:
  - Fixed Issue 215 reported by mrc.mgg:
    - `finite?` and `flfinite?` were returned #t for NaNs.

  - Fixed Issue 217 reported by David Banks:
    - `eqv?` was returned `#t` for `(eqv? 4.0 4)`

  - Various procedures fixed for returning correct exact/inexact value.

  - Fixed Issue 224 reported by David Banks:
    - `hashtable-ref` could cause segfaults.

  - Bytevector buffer port opened by `open-bytevector-output-port` was broken
    when the value outputted multiple times.

  - Fixed some compiler optimization issues.

  - Fixed `equal`.

- R6RS:
  - Fixed `vector-map` problem reported by Eduardo Cavazos:
    - `vector-map` was not handle correctly when given procedure returned
      multiple times. Now `vector-map` defined as vector variant of R6RS
      `map`.

- file-executable?
  - file-executable? now always returns #f in Windows platform.
    See https://github.com/higepon/mosh/issues/65.
