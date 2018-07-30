package rdf

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse.Parser
import org.openrdf.model.{Resource, Statement}
import rdf.readRDF._
import utils.PostProcessXML

import scala.xml.{Elem, NodeSeq}

object diagrams {
  def exampleWithDiagram(f: java.io.File, s:String):NodeSeq = {
    val rdf = Settings.prefixes + "\n" + s
    // val g = parseStringToGraph(rdf)

    //Console.err.println(rdf)
    val dot:String = makeDot(rdf)
    createSVG(dot, f.getCanonicalPath)
    val imgLink = f.getParentFile.getParentFile.toPath().relativize(f.toPath).toString
    <pre>
      {s}
    </pre>
        <img src={imgLink}/>
  }


  def processExamples(e: Elem, imageDir: java.io.File) = PostProcessXML.updateElement2(e, _.label=="example", e => exampleWithDiagram(e.text, imageDir))

  def processOntologies(e: Elem, imageDir: java.io.File) = PostProcessXML.updateElement(e, _.label=="ontology", e => ontologyDiagram((e \\ "@source").text, imageDir))


  def ontologyDiagram(source: String, dir: java.io.File) =
  {
    val o = Schema.fromFile(source)
    val file = new java.io.File(dir.getCanonicalPath + "/" +  java.util.UUID.randomUUID + ".svg")

    val imgLink = file.getParentFile.getParentFile.toPath().relativize(file.toPath).toString
    o.createImage(file.getCanonicalPath)

    <img src={imgLink}/>
  }

  def exampleWithDiagram(rdf: String, imageDir: java.io.File):NodeSeq =
  {
    val imgFile = new java.io.File(imageDir.getCanonicalPath + "/" +  java.util.UUID.randomUUID + ".svg")
    exampleWithDiagram(imgFile, rdf)
  }


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

  def makeLabel(n: String, className: String, dataProperties: Seq[Statement]) = {
    lazy val oMax = dataProperties.map(dp => shortName(dp.getObject).length).max

    //val w = if (dataProperties.isEmpty || oMax < 50) None else Some(scala.xml.Text("4cm"))
    <table BORDER="0" CELLBORDER="0" CELLSPACING="0">
      <tr bgcolor="pink">
        <td bgcolor="lightblue" colspan="2"><i>{n}:{className}</i></td>
      </tr>{dataProperties.map(dp => <tr>
      <td>
        {shortName(dp.getPredicate)}
      </td> <td>{val o = shortName(dp.getObject)
          o.substring(0, Math.min(o.length, 20))
        }</td>
    </tr>)}
    </table>
  }

  def makeIdentifier(str: String) = str.replaceAll("[^A-Za-z0-9:]","")


  def makeDot(seq: Seq[Statement], includeClassInLabel: Boolean = true):String = {
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


    def quote(s:String) =
    {
      s""""${s.replaceAll("\"","")}""""
    }

    val subjectInfo:List[String] = bySubject.toList.map(
      {
        case (s,l) =>
          val n = shortName(s)
          val n1 = makeIdentifier(n)
          val className = l.find(isIsA).map(s => shortName(s.getObject)).getOrElse("UNK")

          val dataProperties = l.filter(isDataProperty)
          val objectProperties = l.filter(isObjectProperty).filter(!isIsA(_))

          val dataPropertyLabelPart = dataProperties.map(dp => s"${shortName(dp.getPredicate)}=${shortName(dp.getObject)}").mkString("\\l")
          val label = if (dataPropertyLabelPart.isEmpty) className else s"$className|$dataPropertyLabelPart"

          val htmlLabel = makeLabel(n,className,dataProperties)

          //  <table BORDER="0" CELLBORDER="0" CELLSPACING="0"><tr bgcolor="pink"><td bgcolor="lightblue" colspan="2"><i>{n}:{className}</i></td></tr>{dataProperties.map(dp => <tr><td>{shortName(dp.getPredicate)}</td><td>{shortName(dp.getObject)}</td></tr>)}</table>

          (s"""\n$n1 [label=<$htmlLabel>]// [label="{$n : $label}"]"""
            ::
            objectProperties.toList.map( o => s"""${n1} -> ${makeIdentifier(shortName(o.getObject))} [ color="#000088", arrowhead=vee, label = ${quote(objectPropertyLabel(o))}] """))
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
        s"""${makeIdentifier(n)} [label=<${makeLabel(n,"Class", Seq.empty)}>]"""
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
    s.foreach(println)
    val dot = makeDot(s)
    // println(makeDot(s))
    createSVG(dot,"test.svg")
  }
}
