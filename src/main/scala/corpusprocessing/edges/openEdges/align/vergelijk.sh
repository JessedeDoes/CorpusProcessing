paste src-test.txt Results/test.out.txt | perl -pe 's/$/\n/' | head -1000 | perl -pe 's/\t/\n/'
