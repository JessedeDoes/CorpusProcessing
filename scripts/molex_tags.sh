echo "<text>";
(echo 'copy (select * from data.distinct_lemma_gigpos) to stdout' | pg6 gig_pro; echo 'copy (select * from data.distinct_lemma_gigpos) to stdout' | pg6 gig_pro) | cut -f1 | perl -pe 's/(.*)/<w pos="$1"\/>/'
echo "</text>";
