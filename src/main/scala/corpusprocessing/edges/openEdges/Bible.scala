package corpusprocessing.edges.openEdges

import java.io.File

case class Bible(bibles: BibleCorpus, language: String, bible: String) {

  val textPath = bibles.baseDir + "Texts/" + language + "/" + bible + "/"

  lazy val books: Array[Book] = {
    val files = new File(textPath).listFiles().filter(_.getName.endsWith(".tsv"))
    files.map(f => {
      Console.err.println(s"Reading bible book from $f")
      val book = readBook(bible, f.getCanonicalPath)
      Console.err.println(s"Finished reading bible book from $f")
      book
    })
  }

  lazy val bookNames = books.map(_.book).toSet

  def readBook(bible: String, bookPath: String): Book = {
    val verses = io.Source.fromFile(bookPath).getLines().zipWithIndex.map({ case (l, i) => {
      val parts = l.split("\t")
      val verseId = parts(0)
      val verse = parts(1)
      val n = i.toString
      val book = new File(bookPath).getName.replaceAll(".tsv", "")
      val ref = VerseRef(bible, book, n)
      Verse(verseId, verse, ref)
    }
    }).toStream
    Book(this, verses)
  }

  def getBook(book: String): Option[Book] = books.find(_.book == book)
}
