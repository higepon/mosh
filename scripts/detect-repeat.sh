#!/bin/zsh

# wrapper of detect-repeat.sh
# usage: ./detect-repeat.sh bench/fib.scm
make _dump_insn_target 2>/dev/null 1>/dev/null
#./test $1 1>/dev/null 2>&1 | scripts/detect-repeat.scm
#./test ./bench/case.scm 1>/dev/null 2>&1 | scripts/detect-repeat.scm

rm -f  ./instruction.log
./test $1
#scripts/detect-repeat.scm ./instruction.log
./scripts/detect-repeat
