package utils

import java.io.FileInputStream

import org.openrdf.model.Statement
import org.openrdf.rio.RDFFormat
import org.openrdf.rio.RDFParser
import org.openrdf.rio.Rio
import org.openrdf.model.Resource
import scala.collection.JavaConverters._

import guru.nidi.graphviz.model.Factory._
import guru.nidi.graphviz.parse._

object readRDF
{
  val rdfParser: RDFParser = Rio.createParser(RDFFormat.TURTLE)

  def parseToGraph(f: String):org.openrdf.model.Graph  = {

    val inputStream = new FileInputStream(f)

    import org.openrdf.rio.helpers.StatementCollector
    val myGraph = new org.openrdf.model.impl.GraphImpl

    val collector = new StatementCollector(myGraph)
    rdfParser.setRDFHandler(collector)

    rdfParser.parse(inputStream, "http://")
    myGraph
  }

  def parseToStatements(url: String): Stream[Statement] = parseToGraph(url).iterator().asScala.toStream

  def shortName(r: org.openrdf.model.Value) = r.stringValue().replaceAll(".*(#|/)", "")
  def shortName(r:  org.openrdf.model.URI) = r.stringValue().replaceAll(".*(#|/)", "")

  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  val prelude =
    """
      |digraph G {
      |  fontname = "Bitstream Vera Sans"
      |    fontsize = 8
      |
      |    node [
      |      fontname = "Bitstream Vera Sans"
      |      fontsize = 8
      |      shape = "record"
      |    ]
      |
      |    edge [
      |      fontname = "Bitstream Vera Sans"
      |      fontsize = 8
      |    ]
    """.stripMargin


  def isObjectProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Resource]
  def isDataProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Literal]
  def isIsA(st:Statement) = st.getPredicate.toString.equals(isA)

  // scala rewrite of convert_to_dot.py from the ontolex github

  def createPNG(dot: String, outFile: String) =
  {
    val s:java.io.StringReader = new java.io.StringReader(dot)
    val g = Parser.read(dot)
    import guru.nidi.graphviz.engine.Graphviz
    import guru.nidi.graphviz.engine.Format
    val viz = Graphviz.fromGraph(g)
    viz.render(Format.PNG).toFile(new java.io.File(outFile))
  }


  def makeDot(s: Seq[Statement]):String = {
    val bySubject = s.groupBy(_.getSubject)

    val subjectInfo:List[String] = bySubject.toList.map(
      {
        case (s,l) =>
          val n = shortName(s)

          val className = l.find(isIsA).map(s => shortName(s.getObject)).getOrElse("UNK")

          val dataProperties = l.filter(isDataProperty)
          val objectProperties = l.filter(isObjectProperty).filter(!isIsA(_))

          val dataPropertyLabelPart = dataProperties.map(dp => s"${shortName(dp.getPredicate)}=${shortName(dp.getObject)}").mkString("\\l")
          val label = if (dataPropertyLabelPart.isEmpty) className else s"$className|$dataPropertyLabelPart"

          (s"""\n$n [label="{$n : $label}"]"""
            ::
            objectProperties.toList.map( o => s"$n -> ${shortName(o.getObject)} [ label = ${shortName(o.getPredicate)}] "))
            .mkString("\n")
      }
    )

    val unseenObjects = s.filter(isObjectProperty)
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
    createPNG(dot,"test.png")
  }
}
