export CLASSPATH=/home/jesse/workspace/xml2rdf/target/scala-2.12/XmlToRdf-assembly-0.1.jar:/home/jesse/workspace/XmlToRdf/target/scala-2.12/XmlToRdf-assembly-0.1.jar
java rdf.diagrams  $1
PLNAAM1=`basename $1 .xml`
PLNAAM2=`basename $PLNAAM1 .rdf`
mv test.svg $PLNAAM2.svg

