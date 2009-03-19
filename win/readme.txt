
requirements
Microsoft Visual Stduio 2008 SP1

getopt.(h|c) ; copied from PostgreSQL

precompiled binary
gmp          ; 4.1.0
onigruma     ; 5.9.1
gc           ; in mosh

ToDo:
some functions are not supported(FFI, fork, ...)
check warning by VC
run test
use onigruma in mosh repository
gmp binary is slow (without asm)

*memo
http://wiki.monaos.org/pukiwiki.php?Mosh%2F%B3%AB%C8%AF%B4%C4%B6%AD%C0%B0%A4%A8%A4%EB

getrusage => use GetSystemTimes


*how to build gc

>cd win/gc
>make_gc


*test

ok:
clos.scm
condition.scm
exception.scm
output-port.scm
record.scm
srfi19.scm
srfi8.scm
unicode.scm

ng:
dbi.scm
ffi.scm
input-output-port.scm
input-port.scm            ; readBytes?
io-error.scm
mysql.scm
shell.scm
srfi-misc.scm
stack-trace1.scm
stack-trace2.scm
stack-trace3.scm
use-foo.scm
