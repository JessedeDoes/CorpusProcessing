package corpusprocessing.CLVN

import java.io.FileWriter

import utils.PostProcessXML

import scala.xml._
import CLVNUtils._

object DiTXML {

  case class Kopje(p: Node) {

    lazy val text = p.text.trim.replaceAll("\\s+", " ")
    //println("-" + text)
    lazy val columns = text.split(":")
    lazy val isOk = (text.contains(":") && text.indexOf(":") <= 20)

    lazy val key = columns(0).trim
    lazy val value = if (columns.size > 1) columns(1).trim else "BUMMER IN $text"
    lazy val descendants = p.descendant
    override def toString() = s"$key -> $value"
  }

  def clean(e: Elem, keep: Node => Boolean):Elem =
  {
    e.copy(child = e.child.filter(keep).map({
      case e1: Elem => clean(e1,keep)
      case x:Node => x
    }))
  }


  def parseOne(f: String): Unit = {
    // Console.err.println(f)

    val doc = XML.load(f)
    val paragraphs = (doc \\ "p")
    val nParagraphs = paragraphs.size

    val kopjes = (doc \\ "p").filter(!_.text.trim.isEmpty).map(p => Kopje(p)).takeWhile(_.isOk)

    val bronVermelding = paragraphs.drop(nParagraphs - 2).filter(p => p.text.trim.startsWith("Bron:"))

    val paginaNummers = paragraphs.filter(p =>
    {
      val hi = p \ "hi"
      hi.nonEmpty && (hi \ "@rend").toString.contains("bold") &&  p.text.trim.matches("[0-9]+")}
    )

    //kopjes.foreach(k => Console.err.println(k))
    //Console.err.println("BRON: " + bronVermelding.map(_.text))

    val kopLoos = clean(doc, x => !(x.label == "teiHeader") && !kopjes.map(_.p).contains(x) && !(bronVermelding.contains(x)))
    val metPeeBees = PostProcessXML.updateElement(kopLoos, p => paginaNummers.contains(p), pageNumber)

    //println(paginaNummers.map(_.text))

    val referentiecode = kopjes.find(_.key.toLowerCase == "referentiecode").map(_.value.replaceAll("Provincie",""))
      .map(s => s.replaceAll("(^DRE-JOO-[0-9]+$)", "$1-TVS")
        .replaceAll(" .*", "")
        .replaceAll("ZEE-AAR-000", "ZEE--AAR-000"))


    val bronOption = referentiecode.flatMap(c => metadata.ditByReference.get(c))

    if (!bronOption.isDefined)
      Console.err.println(s"Missende metadata: $f ($referentiecode) ->  ${bronOption}")

    val fNew = new java.io.File(f).getName.replaceAll("doc$", "tei.xml")
    

    val newTEI =
      <TEI xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0">
        {if (bronOption.isDefined) bronOption.get.header}
        {metPeeBees \\ "text"}
      </TEI>


    val postProcessedDocument = {
      import PostProcessXML._
      sequence(Seq(
        fixIds1,
        cleanRefs,
        PeetjesInDivjes,
        cleanP,
        cleanHi,
        ditPeeBees),
        newTEI)
    }
    val fw = new java.io.FileWriter(TEIpath + fNew)
    fw.write(postProcessedDocument.toString)
    fw.close
  }

  def pageNumber(p:Elem) =
    <pb n={p.text.trim}/>

  import Settings._

  val XMLpath = ditDirectory + "06_DiT-corpus/DiT-corpusvoorJesse/XML/"
  val TEIpath = ditDirectory + "06_DiT-corpus/DiT-corpusvoorJesse/TEI/"
  // /

  lazy val files = new java.io.File(XMLpath).listFiles().map(_.getCanonicalPath)

  def main(args: Array[String]) =
  {
    files.foreach(parseOne)
  }
}
