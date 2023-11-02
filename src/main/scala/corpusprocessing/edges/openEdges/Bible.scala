package corpusprocessing.edges.openEdges

import corpusprocessing.edges.openEdges.Alignments.readAlignments

import java.io.{File, PrintWriter}
import scala.xml.PrettyPrinter

case class Bible(baseDir: String, alignment_files: Set[String]) {

  lazy val alignments = readAlignments(alignment_files.head)

  lazy val language1 = alignments.alignments.head.v1.head.language
  lazy val bible1 = alignments.alignments.head.v1.head.bible

  lazy val language2 = alignments.alignments.head.v2.head.language
  lazy val bible2 = alignments.alignments.head.v2.head.bible


  lazy val books1 = readBooks(alignments, language1, bible1)
  lazy val books2 = readBooks(alignments, language2, bible2)

  lazy val bookPairs: List[(Book, Book)] = books1.flatMap(b1 => {
    val b2 = books2.find(_.book == b1.book)
    println("Looking for" + b1.book)
    println(b2.map(_.book))
    b2.map(b => (b1,b))
  }).toList

  def getTextPath(language: String, bible: String) = baseDir + "Texts/" + language + "/" + bible + "/"

  def readBook(alignments: Alignments, bible: String, bookPath: String): Book = {
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
    Book(alignments, verses)
  }


  def printBooks(toDir: String = "/tmp/Bible/") = {
    val subdir = toDir + "/content/"
    new File(subdir).mkdir()
    (books1 ++ books2).foreach({ book => {
      val f = subdir + book.xmlFileName
      book.printTo(f)
    }})
  }

  def printBookPairs(toDir: String = "/tmp/Bible/") = {
    val subdir = toDir + "/alignment/"
    new File(subdir).mkdir()

     bookPairs.foreach({case (b1,b2) =>
         val f1 = toDir + b1.xmlFileName
         val f2 =  toDir + b2.xmlFileName
         val xml = <teiCorpus xmlns:xi="http://www.w3.org/2001/XInclude" xmlns="http://www.tei-c.org/ns/1.0">
           <teiHeader>
             <fileDesc><titleStmt><title>Link file for {b1} and {b2} </title></titleStmt></fileDesc>
           </teiHeader>
            <xi:include href={"../content/" + b1.xmlFileName}/>
            <xi:include href={"../content/" + b2.xmlFileName}/>
           <standOff>
             <linkGrp>
           {b1.linkXml()}
             </linkGrp>
             <linkGrp>
           {b2.linkXml()}
             </linkGrp>
           </standOff>
         </teiCorpus>

       val pretty = new PrettyPrinter(100, 2)
       val formatted = pretty.format(xml)
       val pw = new PrintWriter(subdir + b1.fileName + "--" + b2.fileName + ".aligments.xml")
       pw.println(formatted)
       pw.close()
     })
  }

  def readBooks(alignments: Alignments, language: String, bible: String, toDir: String = "/tmp/Bible/"): Array[Book] = {
    val dir = getTextPath(language, bible)
    println("Reader from: " + dir)
    val files = new File(dir).listFiles().filter(_.getName.endsWith(".tsv"))

    files.map(f => {
      val book = readBook(alignments, bible, f.getCanonicalPath)
      book
    })
  }
}
