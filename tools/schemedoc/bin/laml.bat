@echo off
rem  By Kurt Normark, normark@cs.auc.dk.
rem  Copied and modified by the LAML installation process - DO NOT EDIT.
rem  Addapted to Win2000 by Christian Sejberg.
rem  This bat file is used from Windows to process a LAML file.
rem  Usage: laml file
rem  The file is without the laml extension.
set LAMLDIR=c:/Users/Kurt/scheme/
rem set Output dir to current directory
set CDIR=%cd%\
rem echo Output dir is now set to "%CDIR%"
c:\programs\plt\mzScheme -v -f %LAMLDIR%laml.init -d %1.laml %1 %CDIR%
echo Done
