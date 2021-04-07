/usr/bin/perl -pe 's/.#160;/ /g' test.svg > test.subst
rsvg-convert --dpi-x 600 --dpi-y 600 test.subst -o test.pdf

