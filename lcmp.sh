./hscool $1 > a.tmp
reflexer $1 > b.tmp
diff a.tmp b.tmp
rm a.tmp b.tmp
