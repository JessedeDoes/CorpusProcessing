python3.10 links.py dialectconstructies.md   > dialectconstructies_linked.md
pandoc dialectconstructies_linked.md -o dialectconstructies.html

(echo '<body style="font-size: 14pt">' ; cat dialectconstructies.html) > /tmp/dinges.html
perl -pe 's/<colgroup.*?<\/colgroup>//gs' /tmp/dinges.html> /tmp/dinges2.html
perl -pe 's/(<table.*?>)/$1<colgroup><col width="30%"><col width="70%"><\/colgroup>/' /tmp/dinges2.html> /tmp/dinges3.html

cp /tmp/dinges3.html dialectconstructies.html
pandoc dialectconstructies.html -o dialectconstructies.pdf
cp * ~/workspace/corpus-frontend-config/GCND/static/Documentatie/
cp * /mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/Documentatie/XPathDoc/
cd ~/workspace/corpora-index-scripts/GCND; bash copy_config.sh settings.ato.sh

