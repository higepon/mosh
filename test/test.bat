echo off
setlocal
if not exist mosh.exe goto notfound
echo ----------------------------------------
echo input-port.scm
mosh.exe test/input-port.scm
if errorlevel 1 goto end
echo Passed all tests
goto end
:notfound
echo mosh.exe not found!
goto end
:end
