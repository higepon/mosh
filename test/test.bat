@echo off
setlocal
cd ..
if not exist mosh.exe goto notfound
echo ----------------------------------------
mosh.exe test/shell.scm
if errorlevel 1 goto end
echo Passed all tests
goto end
:notfound
echo mosh.exe not found!
goto end
:end
