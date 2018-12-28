package rdf

import scala.xml.{Elem, XML}

object addDiagramsToHTML extends App
{

  val stuff = <div>
    <head>The basic attestation model</head>
    <example>
      :e0 a ontolex:LexicalEntry ;
      ontolex:sense :s0 .
      :l0 a ontolex:Form .
      :e0 ontolex:canonicalForm :l0 .
      :l0 ontolex:writtenRep "koe" .
      :f0 a ontolex:Form .
      :f0 ontolex:writtenForm "koei" .
      :e0 ontolex:lexicalForm :f0 .
      :s0 a ontolex:LexicalSense .
      :s0 ontolex:definition  "De koe is een nuttig dier" .
      :a0 a diamant:Attestation .
      :s0 diamant:attestation :a0 .
      :f0 diamant:attestation :a0 .
      :q0 a diamant:Quotation .
      :q0 diamant:quotationText "de koei zei boe" .
      :s0 diamant:citation :q0 .
      :a0 diamant:text :q0 .
      :a0 nif:beginIndex 3 .
      :a0 nif:endIndex 6 .
      :q0 a diamant:DependentTitle .
      :q0 diamant:containedInTitle :t0 .
      :t0 a diamant:Title .
      :t0 diamant:title "Hinderlijke diertjes " .
      :q0 diamant:witness :w0 .
      :q0 diamant:title "Over pissebedden" .
      :w0 a diamant:Witness .
      :w0 diamant:yearFrom 1810 .
      :w0 diamant:yearTo 1812 .
      :q0 diamant:platonicText :p0 .
      :p0 a diamant:PlatonicText .
      :p0 diamant:yearFrom 1810wel .
      :p0 diamant:yearTo 1812 .
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
