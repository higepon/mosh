@echo off
echo "compile cons.cpp"
cl cons.cpp
echo "cons.exe"
cons
echo "echo a | cons"
echo a | cons
echo "cons | echo"
cons | echo.
echo "echo a | cons | echo"
echo a | cons | echo.
