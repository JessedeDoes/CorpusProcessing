package corpusprocessing.edges.openEdges

import corpusprocessing.edges.openEdges.Alignment.readCorrespondences

import java.io.{File, PrintWriter}
import scala.xml.PrettyPrinter


case class Bible(bibles: Bibles, language: String, bible: String) {

  val textPath= bibles.baseDir + "Texts/" + language + "/" + bible + "/"

  lazy val books: Array[Book] = {
    val files = new File(textPath).listFiles().filter(_.getName.endsWith(".tsv"))
    files.map(f => {
      val book = readBook(bible, f.getCanonicalPath)
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

case class Bibles(baseDir: String, alignment_files: Set[String]) {

  lazy val allAlignments = SetOfAlignments(this, alignment_files)


  lazy val bibles: List[Bible] = {
    allAlignments.bibles.map{case (l,b) => Bible(this,l,b)}
  }.toList

  lazy val allBookNames = bibles.flatMap(_.bookNames).toSet.toList.sorted


  def printBooks(toDir: String = "/tmp/Bible/") = {
    new File(toDir).mkdir()
    val subdir = toDir + "/content/"
    new File(subdir).mkdir()

    bibles.foreach(b => {
      val books = b.books
      books.foreach(bk => {
        val f = subdir + bk.xmlFileName
        bk.printTo(f)
      })
    })
  }

  def printBookAlignments(toDir: String = "/tmp/Bible/"): Unit = {
    Console.err.println(s"Collecting alignments for books: $allBookNames")
    new File(toDir).mkdir()
    allBookNames.foreach(bkname => {

      Console.err.println(s"Collecting alignments for book: $bkname")

      new File(toDir).mkdir()
      val books = bibles.map(_.getBook(bkname)).filter(_.nonEmpty).map(_.get)
      val includes = books.map(b => <xi:include href={"../content/" + b.xmlFileName}/>)
      val allCorrespondences = allAlignments.alignments.map(a => {
        val id = a.bible1 + "--" + a.bible2
        val correspondences = a.filterByBook(bkname)
        <standOff type="book-alignment" xml:id={id}>{correspondences.map(_.toXML)}</standOff>
      })

      val xml = <teiCorpus xmlns:xi="http://www.w3.org/2001/XInclude" xmlns="http://www.tei-c.org/ns/1.0">
        <teiHeader>
          <fileDesc>
            <titleStmt>
              <title>Link file for {bkname}</title>
            </titleStmt>
          </fileDesc>
        </teiHeader>
        {includes}
        {allCorrespondences}
      </teiCorpus>
      val subdir = toDir + "/alignments/"
      new File(subdir).mkdir()
      val fileName = subdir + bkname + ".xml"
            // println(formatted)
      val pw = new java.io.PrintWriter(fileName)
      //pw.println(formatted)
      prettyPrinting.prettyScala(pw, xml)
      pw.close()
    })
  }
}
