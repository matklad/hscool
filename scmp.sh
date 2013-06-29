./lp.sh $@|./semant  > a.tmp
./lp.sh $@|refsemant > b.tmp
diff a.tmp b.tmp
rm a.tmp b.tmp
