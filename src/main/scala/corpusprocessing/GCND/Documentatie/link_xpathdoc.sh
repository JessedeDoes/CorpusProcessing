python3.10 links.py dialectconstructies.md   > dialectconstructies_linked.md
pandoc dialectconstructies_linked.md -o dialectconstructies.html
(echo '<body style="font-size: 14pt">' ; cat dialectconstructies.html) > /tmp/dinges.html
cp /tmp/dinges.html dialectconstructies.html

