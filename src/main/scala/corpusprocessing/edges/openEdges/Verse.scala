package corpusprocessing.edges.openEdges

import scala.xml.NodeSeq

import scala.xml._
case class Verse(verseId: String, verse: String, ref: VerseRef) {
  val bible = ref.bible
  val language = ref.language
  val book = ref.book
  val lineNumber = ref.lineNumber
  val xmlId = ref.xmlId

  def toXML(corresp: String) = {
    val corr: Option[Node] = if (corresp.isEmpty) None else Some(Text(corresp))
    <ab type="verse" corresp={corr} xml:lang={language} n={verseId} xml:id={xmlId}>{verse}</ab>
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