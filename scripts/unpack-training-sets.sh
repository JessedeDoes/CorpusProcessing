sourceDir=$1
targetDir=$2
rm -rf $targetDir;
cp -R $sourceDir $targetDir
for d in `find $targetDir -type d | grep [a-z]`;
do
  echo "Unpacking $d";
  (cd $d; gunzip *.gz; rm `ls | pcregrep -v 'partitionInformation|tsv'`)
done
