~/Downloads/apache-any23-cli-2.2/bin/any23 rover -f ntriples *.html  | perl -pe 's/file:.*?(feat|dep|pos).html/http:\/\/rdf.ivdnt.org\/schema\/universaldependencies\/$1/g' > olia.txt
