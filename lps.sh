#!/bin/sh

./lexer $@|./parser|./semant
