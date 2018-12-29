export CLASSPATH=../../../target/scala-2.12/XmlToRdf-assembly-0.1.jar 
java rdf.diagramsInLatex main.tex main.bla.tex
pdflatex main.bla

