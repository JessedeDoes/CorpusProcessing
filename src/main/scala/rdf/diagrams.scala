package rdf

import java.io.PrintWriter

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
    createSVG(dot, f.getCanonicalPath, true)
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

  def createSVG(dot: String, outFile: String, makePDF: Boolean=false) =
  {
    val s:java.io.StringReader = new java.io.StringReader(dot)
    val g = Parser.read(dot)

    val viz = Graphviz.fromGraph(g)
    viz.render(Format.SVG).toFile(new java.io.File(outFile))
    if (makePDF) {
      val pdfOut = outFile.replaceAll("\\.[A-Z-a-z0-9]*$", ".pdf")
      Runtime.getRuntime.exec(s"rsvg-convert --dpi-x 600 --dpi-y 600 $outFile  -o $pdfOut")
    }
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

    val isAs = seq.filter(Settings.isIsA)

    Console.err.println("isAs:")
    isAs.foreach(x => Console.err.println(x))


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

    import Settings.isIsA
    val subjectInfo:List[String] = bySubject.toList.map(
      {
        case (s,l) =>
          val n = shortName(s)
          val n1 = makeIdentifier(n)
          Console.err.println(s"Subject: $s $n / $n1")
          val className = l.find(Settings.isIsA).map(s => shortName(s.getObject)).getOrElse("UNK")

          val dataProperties = l.filter(isDataProperty)
          val objectProperties = l.filter(isObjectProperty).filter(!isIsA(_)).sortBy(_.getPredicate.toString)

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

object diagramsInLatex
{
  import diagrams._

  def exampleWithDiagram(options: String, f: java.io.File, s:String):String = {
    val rdf = Settings.prefixes + "\n" + s
    // val g = parseStringToGraph(rdf)

    //Console.err.println(rdf)
    val dot:String = makeDot(rdf)
    createSVG(dot, f.getCanonicalPath, true)
    val imgLink = f.getParentFile.getParentFile.toPath().relativize(f.toPath).toString
    <pre>
      {s}
    </pre>
        <img src={imgLink}/>
    val pdfName = f.getName.replaceAll("svg$", "pdf")
    s"""\\\\begin\\{verbatim\\}$s\\\\end\\{verbatim\\}\\\\begin\\{figure\\}\\\\includegraphics[$options]{${pdfName}}\\\\label\\{fig:${f.getName}\\}\\\\end\\{figure\\}"""
  }

  def exampleWithDiagram(options: String, imagePrefix: String, rdf: String, imageDir: java.io.File):String =
  {
    Console.err.println(rdf)
    val imgFile = new java.io.File(imageDir.getCanonicalPath + "/" +  imagePrefix + ".svg")
    exampleWithDiagram(options, imgFile, rdf)
  }

  def main(args: Array[String]): Unit = {
    val fIn = args(0)
    val dirForfIn = new java.io.File(fIn).getCanonicalFile.getParentFile
    Console.err.println("image dir " + dirForfIn)
    val contents = scala.io.Source.fromFile(fIn).mkString
    Console.err.println(contents.length)
    val r1 = new scala.util.matching.Regex("(?s)\\\\begin\\{rdf\\}\\[(.*?),prefix=(.*?)\\](.*?)\\\\end\\{rdf\\}")
    val newContents = r1.replaceAllIn(contents, m => s"${exampleWithDiagram(m.group(1), m.group(2), m.group(3),dirForfIn)}")
    val fOut = args(1)
    ((x:PrintWriter) => {x.write(newContents); x.close }).apply( new PrintWriter(fOut))
    //System.out.println(newContents)
  }
}
