MNL=/mnt/Projecten/Corpora/Historische_Corpora/MNL-TEI/Nederlabversie
rm -rf $MNL/TaggedV2/*
cd /home/jesse/workspace/DutchTagger
bash scripts/babtag.sh $MNL/Metaculous/  $MNL/TaggedV2/
cd -
export CLASSPATH=./target/scala-2.12/XmlToRdf-assembly-0.1.jar
java -Xmx10g corpusprocessing.enDanMNL
