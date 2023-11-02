package corpusprocessing.edges.openEdges

case class VerseRef(bible: String, book: String, lineNumber: String) {
  val language = bible.replaceAll("_.*", "")
  val xmlId = s"$bible.$book.$lineNumber"
}
