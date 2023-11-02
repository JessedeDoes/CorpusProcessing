perl tokenize.pl < src-all.txt >  /tmp/src.txt
perl tokenize.pl < target-all.txt >  /tmp/target.txt
paste /tmp/src.txt /tmp/target.txt | perl -pe 's/\t/ ||| /' > /tmp/bible.alignMe
fast_align -i /tmp/bible.alignMe -N -d -o -v -I 10 > forward.align
export JAVA_OPTS=-Xmx10g
scala decodeAlign.scala forward.align /tmp/src.txt /tmp/target.txt  > alignmentLexicon.txt
grep -v '#' alignmentLexicon.txt | sort | uniq -c > lex.sorted
grep -v '#' alignmentLexicon.txt | sort | uniq -c | perl -pe 's/^ *//' | perl -pe 's/ /\t/' > lex.sorted
