#!/bin/bash

./test $1 2> /tmp/hoge
echo "** $1"
echo ">|scheme|"
head -n 40 $1
echo "||<"
echo "*** instructions"
echo ">|scheme|"
./scripts/count-insn.scm < /tmp/hoge
echo "||<"
echo "*** patterns"
echo ">|scheme|"
./scripts/insn-bridge.scm < /tmp/hoge | ./scripts/detect-repeat.scm
echo "||<"
rm -f /tmp/hoge

