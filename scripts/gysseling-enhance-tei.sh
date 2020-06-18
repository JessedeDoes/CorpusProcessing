export CLASSPATH=target/scala-2.12/Hilex-assembly-1.0.jar
SOURCEDIR=/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-tei
TARGETDIR=/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-enhanced-tei


SOURCEDIR=/mnt/Projecten/corpora/Historische_Corpora/CorpusGysseling//Nederlabversie/
TEMPDIR=/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-enhanced-tei
TARGETDIR=/mnt/Projecten/corpora/Historische_Corpora/CorpusGysseling//TeIndexeren/
#MAPPED=/tmp/Translated/
#WITHPARTS=/tmp/WithPart
#mkdir $RENAMED
export JAR1=./target/scala-2.12/XmlToRdf-assembly-0.1.jar
#java -Xmx8g tei2folia.Milestone2Part $SOURCEDIR $WITHPARTS
(java -Xmx10g -cp $JAR1 corpusprocessing.gysseling.mapMiddelnederlandseTagsGys $SOURCEDIR $TEMPDIR)
#export CLASSPATH=./target/scala-2.12/Hilex-assembly-1.0.jar
java -Xmx8g -cp $JAR1 corpusprocessing.enNuGysseling
