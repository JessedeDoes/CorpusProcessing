package corpusprocessing.edges.openEdges

import scala.xml.NodeSeq

import scala.xml._
import scala.util.{Try,Failure,Success}
case class Verse(verseId: String, verse: String, ref: VerseRef) {
  val bible = ref.bible
  val language = ref.language
  val book = ref.book
  val lineNumber = ref.lineNumber
  val xmlId = ref.xmlId

  lazy val parsedVerse: Object = {
    val v1 = verse.replaceAll("(^|\\s)\\{(\\S+)\\}($|\\s+)", "$1<add>$2</add>$3")
    val v2 = v1.replaceAll("\\{(\\S+)\\}", "<expan>$1</expan>")
    val v3 = v2.replaceAll("\\[([^\\[\\]]+)\\]", "<add type='squarebrackets'>$1</add>")
    val v4 = s"<boiler>$v3</boiler>"
    if (verse.contains("{")) println(v3)
    Try{XML.loadString(v4).child} match {
      case Success(x) => x
      case _ => verse
    }
  }

  def toXML(corresp: String) = {
    val corr: Option[Node] = if (corresp.isEmpty) None else Some(Text(corresp))
    <ab type="verse" corresp={corr} xml:lang={language} n={verseId} xml:id={xmlId}>{parsedVerse}</ab>
  }
}

object Verse {
  def verse(bible: String, bookverse: String): VerseRef = {

    val parts = bookverse.replaceAll("\\.tsv", "") split ("\\.")
    val book = parts(0)
    val verse = parts(1)
    VerseRef(bible, book, verse)
  }
}