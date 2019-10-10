package rdf

import scala.xml.{Elem, XML}

object addDiagramsToHTML extends App
{

  val stuff = <div>
    <head>The basic attestation model</head>

    <example>
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://purl.org/vocab/frbr/core#Manifestation&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://rdf.ivdnt.org/schema/diamant#Quotation&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://purl.org/vocab/frbr/core#embodimentOf&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/expression/WNT/332819&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://rdf.ivdnt.org/schema/diamant#dictionary&gt;	"WNT" .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://rdf.ivdnt.org/schema/diamant#gtbId&gt;	"M030758.eg.31647" .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://rdf.ivdnt.org/schema/diamant#witnessYearFrom&gt;	"1621"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; &lt;http://rdf.ivdnt.org/schema/diamant#witnessYearTo&gt;	"1621"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/oa_source/2108540&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#value&gt;	"Meene met dien man wat te doen sal zijn; voechde bij sijn seggen dat hij wel weet, dat men licht yemant de eat aen het been kan werpen," .
      &lt;http://rdf.ivdnt.org/lexica/diamant/expression/WNT/332819&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://purl.org/vocab/frbr/core#Expression&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/expression/WNT/332819&gt; &lt;http://dublincore.org/2012/06/14/dcterms.ttl#creator&gt;	"N. V. REIGERSB." .
      &lt;http://rdf.ivdnt.org/lexica/diamant/expression/WNT/332819&gt; &lt;http://dublincore.org/2012/06/14/dcterms.ttl#title&gt;	"Br." .
      &lt;http://rdf.ivdnt.org/lexica/diamant/expression/WNT/332819&gt; &lt;http://purl.org/vocab/frbr/core#embodiment&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/annotation/2108540&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://www.w3.org/ns/oa#Annotation&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/annotation/2108540&gt; &lt;http://www.w3.org/ns/oa#hasBody&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/annotation/2108540&gt; &lt;http://www.w3.org/ns/oa#hasTarget&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/oa_target/2108540&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://rdf.ivdnt.org/schema/diamant#Attestation&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://purl.org/spar/cito/hasCitedEntity&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://purl.org/spar/cito/hasCitingEntity&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://rdf.ivdnt.org/schema/diamant#locus&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/locus/2108540&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://rdf.ivdnt.org/schema/diamant#quotation&gt;	"Meene met dien man wat te doen sal zijn; voechde bij sijn seggen dat hij wel weet, dat men licht yemant de eat aen het been kan werpen," .
      &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; &lt;http://rdf.ivdnt.org/schema/diamant#text&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/quotation/WNT/332819&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/entry/WNT/M030758&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://www.w3.org/ns/lemon/ontolex#LexicalEntry&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/entry/WNT/M030758&gt; &lt;http://www.w3.org/ns/lemon/ontolex#sense&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/selector/2108540&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://www.w3.org/ns/oa#TextPositionSelector&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/selector/2108540&gt; &lt;http://www.w3.org/ns/oa#end&gt;	"110"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/selector/2108540&gt; &lt;http://www.w3.org/ns/oa#start&gt;	"107"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/oa_target/2108540&gt; &lt;http://www.w3.org/ns/oa#hasSelector&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/selector/2108540&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/oa_target/2108540&gt; &lt;http://www.w3.org/ns/oa#hasSource&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/oa_source/2108540&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://www.w3.org/ns/lemon/ontolex#LexicalSense&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://www.w3.org/2000/01/rdf-schema#label&gt;	"V.—" .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://lemon-model.net/lemon#definition&gt; &lt;http://rdf.ivdnt.org/definition/WNT/M030758.bet.207&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#attestation&gt; &lt;http://rdf.ivdnt.org/lexica/diamant/attestation/2108540&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#dictionary&gt;	"WNT" .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#gtbId&gt;	"M030758.bet.207" .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#isCoreSense&gt;	"false"^^&lt;http://www.w3.org/2001/XMLSchema#boolean&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#polyLexical&gt;	"true"^^&lt;http://www.w3.org/2001/XMLSchema#boolean&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#senseOrder&gt;	"216"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/sense/WNT/M030758.bet.207&gt; &lt;http://www.w3.org/2004/02/skos/core#definition&gt;	"Iemand een kat (of de kat) aan het been jagen (HARREB. 1, 40 b [1858]) of werpen (naar fra. jeter le chat aux jambes à quelqu'un, of de quelqu'un), iemand in moeilijkheden brengen." .
      &lt;http://rdf.ivdnt.org/definition/WNT/M030758.bet.207&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://lemon-model.net/lemon#SenseDefinition&gt; .
      &lt;http://rdf.ivdnt.org/definition/WNT/M030758.bet.207&gt; &lt;http://rdf.ivdnt.org/schema/diamant#definitionText&gt;	"Iemand een kat (of de kat) aan het been jagen (HARREB. 1, 40 b [1858]) of werpen (naar fra. jeter le chat aux jambes à quelqu'un, of de quelqu'un), iemand in moeilijkheden brengen." .
      &lt;http://rdf.ivdnt.org/lexica/diamant/locus/2108540&gt; &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#type&gt; &lt;http://rdf.ivdnt.org/schema/diamant#Locus&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/locus/2108540&gt; &lt;http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#beginIndex&gt;	"107"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
      &lt;http://rdf.ivdnt.org/lexica/diamant/locus/2108540&gt; &lt;http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#endIndex&gt;	"110"^^&lt;http://www.w3.org/2001/XMLSchema#integer&gt; .
        </example>
  </div>

  val document = if (args.size > 0) XML.load(args(0)) else stuff
  val outputDocument = if (args.size > 0) args(0).replaceAll(".html$", ".diagrams.html") else "aap.html"
  val imageDirectory = if (args.size > 0) {
    val dir = new java.io.File(args(0)).getParentFile
    val subdir = new java.io.File(dir.getAbsolutePath + "/" + "images")
    if (!subdir.isDirectory) subdir.mkdir()
    subdir
  } else {
    val d = new java.io.File("./temp")
    if (!d.isDirectory) d.mkdir()
    d
  }

  val xstuff = diagrams.processOntologies(diagrams.processExamples(document, imageDirectory).asInstanceOf[Elem],imageDirectory)
  //Console.err.println(xstuff)
  XML.save(outputDocument, xstuff, "UTF-8")
}
