export CLASSPATH=../../../target/scala-2.12/XmlToRdf-assembly-0.1.jar 
java rdf.diagramsInLatex clariah_lexica.tex clariah_lexica.diagrams.tex
for file in `ls *.svg`; 
do
  perl -pe 's/&#160;/ /g' $file > /tmp/$file
  pdfOut=`basename $file .svg`.pdf
  rsvg-convert --dpi-x 600 --dpi-y 600 /tmp/$file  -o $pdfOut
done
pdflatex clariah_lexica.diagrams.tex 

