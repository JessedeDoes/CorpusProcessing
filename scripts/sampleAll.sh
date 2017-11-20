export DIR=../nederlab-linguistic-enrichment/experiment3/
export DIR=/home/INL/does/mount/Projecten/Nederlab/Tagging/nederlab-linguistic-enrichment

for x in `find $DIR -name "*.xml.gz"`;
do
  echo $x;
  y=`echo $x | perl -pe 's/folia/sampled.tei/' | perl -pe 's/.gz$//'`
  echo $y;
  java folia.PrepareSample $x > $y
done
