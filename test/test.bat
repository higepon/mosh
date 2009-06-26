echo off
setlocal
if not exist mosh.exe goto notfound
echo ----------------------------------------
echo base
mosh.exe -5 all-tests.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo input-port
mosh.exe test/input-port.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo output-port
mosh.exe test/output-port.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo input/output-port
mosh.exe test/input-output-port.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo clos
mosh.exe test/clos.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo dbi
mosh.exe test/dbi.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo ffi
mosh.exe test/ffi.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo shell
mosh.exe test/shell.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo condition
mosh.exe test/condition.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo exception
mosh.exe test/exception.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo io-error
mosh.exe test/io-error.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo record
mosh.exe test/record.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo srfi8
mosh.exe test/srfi8.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo srfi19
mosh.exe test/srfi19.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo unicode
mosh.exe test/unicode.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo srfi-misc
mosh.exe test/srfi-misc.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo socket
mosh.exe test/socket.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo concurrent
mosh.exe test/concurrent.scm
if errorlevel 1 goto end
echo ----------------------------------------
echo concurrent-crash
mosh.exe test/concurrent-crash.scm
if errorlevel 1 goto end

echo ----------------------------------------
echo R6RS Test Suite
cd r6rs-test-suite
..\mosh.exe tests\r6rs\run-via-eval.sps
if errorlevel 1 goto end
cd ..
echo Passed all tests
goto end
:notfound
echo mosh.exe not found!
goto end
:end
