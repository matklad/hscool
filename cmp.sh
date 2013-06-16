#!/bin/sh
./lexer $1 | refparser > a.out
./lexer $1 | ./parser > b.out 2> /dev/null
diff a.out b.out
rm *.out
