package corpusprocessing.edges.openEdges

import scala.annotation.tailrec
import scala.xml.{Elem, PrettyPrinter}


case class Chapter(cid: String, verses: Seq[Verse]) {

}
object Chapter {
  def orderedGroupBy[T, P](seq: Traversable[T])(f: T => P): Seq[(P, Traversable[T])] = {
    @tailrec
    def accumulator(seq: Traversable[T], f: T => P, res: List[(P, Traversable[T])]): Seq[(P, Traversable[T])] = seq.headOption match {
      case None => res.reverse
      case Some(h) => {
        val key = f(h)
        val subseq = seq.takeWhile(f(_) == key)
        accumulator(seq.drop(subseq.size), f, (key -> subseq) :: res)
      }
    }

    accumulator(seq, f, Nil)
  }

  def chapterise(verses: Seq[Verse]): Seq[Chapter] = {
    val groups = orderedGroupBy[Verse, String](verses)(v => v.verseId.replaceAll("\\.[0-9]+$",""))
    groups.map{case (cid,l) => Chapter(cid,l.toSeq)}
  }
}
case class Book(myBible: Bible, verses: Stream[Verse]) {
  val bibles = myBible.bibles
  val ref = verses.head.ref
  val bible: String = ref.bible
  val language = ref.language
  val book = ref.book
  val pid = s"$bible.$book"
  val alignments: SetOfAlignments = bibles.allAlignments
  override def toString = s"$language.$bible.$book"
  val links: Seq[(VerseRef, Set[VerseRef])] = verses.map(v => (v.ref -> alignments.alignments.flatMap(a => a.aligned(v.ref))))

  def link(a: String, b: String): Elem = <link type="sentence-alignment" target={s"#$a #$b"}/>   // <link from={r.xmlId} to={r1.xmlId}/>
  def link(a: VerseRef, b: VerseRef): Elem  = link(a.xmlId, b.xmlId)
  def linkXml(): Seq[Elem] = links.flatMap({ case (r, s) => s.map(r1 => link(r,r1)) })

  def linkXml(v: Verse): Seq[Elem] = links.filter(_._1.xmlId == v.ref.xmlId).flatMap({ case (r, s) => s.map(r1 => link(r,r1)) })

  def linkCorresp(v: Verse): String = links.filter(_._1.xmlId == v.ref.xmlId).flatMap({ case (r, s) =>  s.map(r1 => s"#${r1.xmlId}") }).toSet.mkString(" ")
  def printTo(f: String, includeLinks: Boolean = false)  = {

    Console.err.println(s"Printing book $book from bible $bible ....")
    val pretty = new PrettyPrinter(1000, 2)
    val x = this.toXML(includeLinks)
    lazy val formatted = pretty.format(x)
    // println(formatted)
    val pw = new java.io.PrintWriter(f)
    //pw.println(formatted)
    prettyPrinting.prettyScala(pw, x)
    pw.close()
  }

  def headerFields = {
    val year = bible.replaceAll(".*([0-9]{4}).*", "$1")
    val bibleName = bible.replaceAll(".*_([0-9]{4})_(.*)", "$2 $1")

    Map("witnessYearLevel2From" -> year, "titleLevel2" -> bibleName, "titleLevel1" -> s"$book ($bibleName)", "pid" -> pid)
  }

  def interpjes = headerFields.map({case (n,v) => <interpGrp type={n}><interp>{v}</interp></interpGrp>})
  lazy val chapters = Chapter.chapterise(verses)
  val chapterise: Boolean = true
  def toXML(includeLinks: Boolean = false): Elem = {
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>{this.toString}</title>
            <respStmt>
              <resp>Conversion, encoding, enrichment</resp>
              <name>Instituut voor de Nederlandse Taal</name>
            </respStmt>
          </titleStmt>
          <publicationStmt>
            <publisher>Instituut voor de Nederlandse Taal</publisher>
            <pubPlace>Leiden</pubPlace>
            <date>2023</date>
            <availability>
              <p>Via online dictionary application: https://edges.ivdnt.org</p>
            </availability>
          </publicationStmt>
        </fileDesc>
        <sourceDesc><bibl type="intMetadata">{interpjes}</bibl></sourceDesc>
      </teiHeader>
      <text xml:lang={language} xml:id={s"$bible.$book"}>
        <body>
          <div type="book">
            {if (chapterise)
              chapters.map(c => <div type="chapter" n={bible + "." + c.cid}>{c.verses.map(v => v.toXML(corresp=linkCorresp(v)))}</div>)
            else verses.map(v => v.toXML(corresp=linkCorresp(v)))}
          </div>
        </body>
      </text>
        {if (includeLinks) <linkGrp>{linkXml}</linkGrp> }
    </TEI>
  }

  val xmlFileName = s"$bible.$book.xml"
  val fileName = s"$bible.$book"
}
