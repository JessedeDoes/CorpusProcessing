package utils

import java.io.{FileInputStream, StringReader}

import org.openrdf.model.Statement
import org.openrdf.rio.RDFFormat
import org.openrdf.rio.RDFParser
import org.openrdf.rio.Rio
import org.openrdf.model.Resource

import scala.collection.JavaConverters._
import guru.nidi.graphviz.model.Factory._
import guru.nidi.graphviz.parse._
import guru.nidi.graphviz.engine.{Format, Graphviz, GraphvizJdkEngine}
import org.postgresql.util.ReaderInputStream
import org.openrdf.model.Graph

import scala.util.Try
import scala.xml._

object Settings
{
  val prefixes =  s"""
                    |@prefix dc: <http://purl.org/dc/elements/1.1/#> .
                    |@prefix dcterms: <http://dublincore.org/2012/06/14/dcterms.ttl#> .
                    |@prefix owl: <http://www.w3.org/2002/07/owl#> .
                    |@prefix pwn: <http://wordnet-rdf.princeton.edu/ontology#> .
                    |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    |@prefix xml: <http://www.w3.org/XML/1998/namespace> .
                    |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                    |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                    |@prefix quest: <http://obda.org/quest#> .
                    |@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
                    |@prefix diamant: <http://rdf.ivdnt.org/schema/diamant#> .
                    |@prefix lemon: <http://lemon-model.net/lemon#> .
                    |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .
                    |@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .
                    |@prefix nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#> .
                    |@prefix olia_top: <http://purl.org/olia/olia-top.owl> .
                    |@prefix prov: <http://www.w3.org/ns/prov#> .
                    |@prefix ud: <http://universaldependencies.org/u/> .
                    |@prefix : <#> .\n""".stripMargin
}


object readRDF
{
  val formatsToTry = List(RDFFormat.RDFXML, RDFFormat.NTRIPLES, RDFFormat.N3, RDFFormat.NQUADS, RDFFormat.TURTLE, RDFFormat.JSONLD, RDFFormat.RDFJSON)

  val rdfParsers = formatsToTry.map(Rio.createParser(_)).toStream

  //val rdfParser: RDFParser = Rio.createParser(RDFFormat.TURTLE)

  def parseToGraph(f: java.io.File):Graph = parseToGraph(() => new FileInputStream(f))

  def parseToGraph(fileName: String):Graph = parseToGraph(new java.io.File(fileName))

  def parseStringToGraph(s: String):Graph =
  {
    def inputStream():java.io.InputStream = {
      val sr = new StringReader(s)

      val x = new ReaderInputStream(sr)
      x
    }
    parseToGraph(() => inputStream)
  }

  def parseToGraph(inputStream: ()=>java.io.InputStream):org.openrdf.model.Graph  = {
    import org.openrdf.rio.helpers.StatementCollector


    val attempts = rdfParsers.map(
      rdfParser => {
        Try({
        val myGraph = new org.openrdf.model.impl.GraphImpl

        val collector = new StatementCollector(myGraph)

        rdfParser.setRDFHandler(collector)
        rdfParser.parse(inputStream(), "http://")

        myGraph})
      })
    attempts.find(_.isSuccess).map(_.asInstanceOf[scala.util.Success[Graph]].value).get
  }

  def exampleWithDiagram(f: String, s:String):NodeSeq = {
    val rdf = Settings.prefixes + "\n" + s
    // val g = parseStringToGraph(rdf)

    Console.err.println(rdf)
    val dot:String = makeDot(rdf)
    createSVG(dot, f)
    <pre>
      {s}
    </pre>
    <img src={f}/>
  }


  def processExamples(e: Elem) = PostProcessXML.updateElement2(e, _.label=="example", e => exampleWithDiagram(e.text))

  def exampleWithDiagram(rdf: String):NodeSeq =
  {
    val imgFile = "temp/" + java.util.UUID.randomUUID + ".svg"
    exampleWithDiagram(imgFile, rdf)
  }

  def parseToStatements(url: String): Stream[Statement] = parseToGraph(url).iterator().asScala.toStream
  def parseStringToStatements(rdf: String): Stream[Statement] = parseStringToGraph(rdf).iterator().asScala.toStream

  def shortName(r: org.openrdf.model.Value) = r.stringValue().replaceAll(".*(#|/)", "")
  def shortName(r:  org.openrdf.model.URI) = r.stringValue().replaceAll(".*(#|/)", "")

  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  val prelude =
    """
      |digraph G {
      |  //fontname = "Bitstream Vera Sans"
      |    fontsize = 10
      |
      |    node [
      |      //fontname = "Bitstream Vera Sans"
      |      fontsize = 10
      |      shape = rectangle
      |      style="rounded"
      |      //fillcolor = "#40e0d0"
      |    ]
      |
      |    edge [
      |      //fontname = "Bitstream Vera Sans"
      |      fontsize = 8
      |    ]
    """.stripMargin


  def isObjectProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Resource]
  def isDataProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Literal]
  def isIsA(st:Statement) = st.getPredicate.toString.equals(isA)

  // scala rewrite of convert_to_dot.py from the ontolex github

  def createSVG(dot: String, outFile: String) =
  {
    val s:java.io.StringReader = new java.io.StringReader(dot)
    val g = Parser.read(dot)

    val viz = Graphviz.fromGraph(g)
    viz.render(Format.SVG).toFile(new java.io.File(outFile))
  }

  def makeDot(rdf: String):String =
  {
    makeDot(parseStringToStatements(rdf))
  }

  def makeDot(seq: Seq[Statement]):String = {
    val bySubject = seq.groupBy(_.getSubject)

    val isAs = seq.filter(isIsA)

    isAs.foreach(println)

    def objectPropertyLabel(s: Statement) =
    {
      val n = shortName(s.getPredicate)
      if (s.getObject == s.getSubject)
        """"""" + n.replaceAll(".", "$0\\\\n") + """""""
      else
        n
    }

    val subjectInfo:List[String] = bySubject.toList.map(
      {
        case (s,l) =>
          val n = shortName(s)

          val className = l.find(isIsA).map(s => shortName(s.getObject)).getOrElse("UNK")

          val dataProperties = l.filter(isDataProperty)
          val objectProperties = l.filter(isObjectProperty).filter(!isIsA(_))

          val dataPropertyLabelPart = dataProperties.map(dp => s"${shortName(dp.getPredicate)}=${shortName(dp.getObject)}").mkString("\\l")
          val label = if (dataPropertyLabelPart.isEmpty) className else s"$className|$dataPropertyLabelPart"

          val htmlLabel = <table BORDER="0" CELLBORDER="0" CELLSPACING="0"><tr bgcolor="pink"><td bgcolor="lightblue" colspan="2"><i>{n}:{className}</i></td></tr>{dataProperties.map(dp => <tr><td>{shortName(dp.getPredicate)}</td><td>{shortName(dp.getObject)}</td></tr>)}</table>

          (s"""\n$n [label=<$htmlLabel>]// [label="{$n : $label}"]"""
            ::
            objectProperties.toList.map( o => s"""$n -> ${shortName(o.getObject)} [ color="#000088", arrowhead=vee, label = ${objectPropertyLabel(o)}] """))
            .mkString("\n")
      }
    )

    val unseenObjects = seq.filter(isObjectProperty)
      .filter(!isIsA(_))
      .map(_.getObject)
      .map(_.asInstanceOf[Resource])
      .toSet
      .diff(bySubject.keySet)

    val objectInfo:List[String] = unseenObjects.map(
      o =>
      {
        val n = shortName(o)
        s"""$n [label = "{$n : UNK}"]"""
      }
    ).toList


      s"""
         |$prelude
         ${subjectInfo.mkString("\n")}
         ${objectInfo.mkString("\n")}
         |}""".stripMargin
  }

  def main(args: Array[String]):Unit = {
    //println(prelude)
    val s = parseToStatements(args(0))
    val dot = makeDot(s)

    println(makeDot(s))
    createSVG(dot,"test.png")
  }
}

import readRDF._
object example extends App
{
  import readRDF.exampleWithDiagram

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
      :q0 a diamant:Text .
      :q0 diamant:quotationText "de koei zei boe" .
      :s0 diamant:citation :q0 .
      :a0 diamant:text :q0 .
      :a0 nif:beginIndex 3 .
      :a0 nif:endIndex 6 .
</example>
  </div>

  val xstuff = processExamples(stuff).asInstanceOf[Elem]
  Console.err.println(xstuff)
  XML.save("aap.html", xstuff, "UTF-8")
}