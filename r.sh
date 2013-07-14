#!/bin/sh

./lexer $@|./parser|./semant| ./cgen > tmp/out.s && spim tmp/out.s
