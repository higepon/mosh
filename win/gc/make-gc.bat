@echo off
echo make gc library
pushd ..\..\gc-7.1
nmake nodebug=1 -f NT_STATIC_THREADS_MAKEFILE
copy gc.lib ..\win\lib
popd
