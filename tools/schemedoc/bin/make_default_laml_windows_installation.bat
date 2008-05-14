@echo off
rem Does the LAML installation and configuration job on windows
rem provided that PLT MzScheme 300+ is used, and that both LAML and
rem PLT are located at the preferred, default locations in the file system.
rem LAML location: programs\laml
rem PLT location:  programs\plt
cd c:\programs\laml\laml-config
c:\programs\plt\MzScheme -v -f laml-config.scm
echo Done
pause