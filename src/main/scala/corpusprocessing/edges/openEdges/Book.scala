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
case class Book(alignments: Alignments, verses: Stream[Verse]) {
  val ref = verses.head.ref
  val bible = ref.bible
  val language = ref.language
  val book = ref.book

  override def toString = s"$language.$bible.$book"
  val links: Seq[(VerseRef, Set[VerseRef])] = verses.map(v => (v.ref -> alignments.aligned(v.ref)))

  def link(a: String, b: String): Elem = <link type="sentence-alignment" target={s"#$a #$b"}/>   // <link from={r.xmlId} to={r1.xmlId}/>
  def link(a: VerseRef, b: VerseRef): Elem  = link(a.xmlId, b.xmlId)
  def linkXml(): Seq[Elem] = links.flatMap({ case (r, s) => s.map(r1 => link(r,r1)) })

  def linkXml(v: Verse): Seq[Elem] = links.filter(_._1.xmlId == v.ref.xmlId).flatMap({ case (r, s) => s.map(r1 => link(r,r1)) })

  def linkCorresp(v: Verse): String = links.filter(_._1.xmlId == v.ref.xmlId).flatMap({ case (r, s) =>  s.map(r1 => s"#${r1.xmlId}") }).toSet.mkString(" ")
  def printTo(f: String, includeLinks: Boolean = false)  = {
    val pretty = new PrettyPrinter(1000, 2)
    val formatted = pretty.format(this.toXML(includeLinks))
    // println(formatted)
    val pw = new java.io.PrintWriter(f)
    pw.println(formatted)
    pw.close()
  }

  lazy val chapters = Chapter.chapterise(verses)
  val chapterise: Boolean = true
  def toXML(includeLinks: Boolean = false) = {
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc><titleStmt><title>{this.toString}</title></titleStmt></fileDesc>
      </teiHeader>
      <text xml:lang={language} xml:id={s"$bible.$book"}>
        <body>
          <div type="book">
            {if (chapterise)
              chapters.map(c => <div type="chapter" id={c.cid}>{c.verses.map(v => v.toXML(corresp=linkCorresp(v)))}</div>)
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
