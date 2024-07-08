package corpusprocessing.edges.openEdges

import corpusprocessing.edges.openEdges.Verse.verse

import java.io.File

case class Correspondence(v1: Set[VerseRef], v2: Set[VerseRef]) {

  def converse() = Correspondence(v2,v1)
  def toXML = {
    val refPairs = v1.flatMap(v => v2.map(w => v -> w))
    <linkGrp n={refPairs.size.toString}>
      {refPairs.map { case (r1, r2) => <link type="verse-alignment" target={s"#${r1.xmlId} #${r2.xmlId}"}/> }}
    </linkGrp>
      }
   lazy val book  = v1.headOption.map(_.book).getOrElse("no_book_how_bad")
}

object Alignment {
  def readCorrespondences(path: String) = {
    Console.err.println(s"Reading alignment from $path...")
    val name = new java.io.File(path).getName
    val parts = name.split("-")
    val b1 = parts(0).replaceAll("\\.tsv", "")
    val b2 = parts(1).replaceAll("\\.tsv", "")

    val source = io.Source.fromFile(path)
    val correspondences = source.getLines().map(l => {
      val parts = l.split("\\t")
      val verses_1 = parts(0).split(",").map(x => verse(b1, x)).toSet
      val verses_2 = parts(1).split(",").map(x => verse(b2, x)).toSet
      Correspondence(verses_1, verses_2)
    }).filter(x => x.v1.nonEmpty && x.v2.nonEmpty).toSet
    source.close()
    Alignment(correspondences)
  }
  import scala.xml._
  def readCorrespondencesFromTEI(f: File) = {

    val d = XML.loadFile(f)
    val correspondences = (d \\ "linkGrp").map(
      lg => {
        val links = (lg \ "link").filter(x => (x \ "@type").text == "verse-alignment").map(l => {
          val c = (l \ "@target").text.split("\\s+").map(_.replaceAll("#", ""))
          c(0) -> c(1)
        })

      }
    )
  }
}

case class Alignment(correspondences: Set[Correspondence]) {
  lazy val index1: Map[String, Set[Correspondence]] = correspondences.flatMap(a => a.v1.map(r => r.xmlId -> a)).groupBy(_._1).mapValues(_.map(_._2))
  lazy val index2: Map[String, Set[Correspondence]] = correspondences.flatMap(a => a.v2.map(r => r.xmlId -> a)).groupBy(_._1).mapValues(_.map(_._2))

  def converse = Alignment(correspondences.map{ _.converse()})
  def isSimplyLinked(id: String) = {
    val others = aligned(id)
    others.size == 1 && aligned(others.head.xmlId).size == 1
  }

  def nonEmpty = correspondences.nonEmpty

  def aligned(id: String) = {
    val a1 = index1.getOrElse(id, Set[Correspondence]())
    val a2 = index2.getOrElse(id, Set[Correspondence]())
    if (a1.nonEmpty) {
      a1.flatMap(_.v2)
    } else a2.flatMap(_.v1)
  }
  def aligned(v: VerseRef): Set[VerseRef] = {
    val id = v.xmlId
    aligned(id)
  }

  lazy val language1 = correspondences.head.v1.head.language
  lazy val bible1 = correspondences.head.v1.head.bible

  lazy val language2 = correspondences.head.v2.head.language
  lazy val bible2 = correspondences.head.v2.head.bible

  lazy val bibles = Set(language1 -> bible1, language2 -> bible2)

  def filterByBook(bookName: String): Set[Correspondence] = correspondences.filter(_.v1.exists(_.book == bookName))
}

case class SetOfAlignments(bible: BibleCorpus,  paths: Set[String], verseAlignedTEIDir: Option[String] = None) {
  lazy val alignmentsOneWay: Set[Alignment] =
    if (verseAlignedTEIDir.nonEmpty) {
      val xmlFiles = new java.io.File(verseAlignedTEIDir.get).listFiles().filter(_.getName.endsWith(".xml"))
      //xmlFiles.map(Alignment.readCorrespondencesFromAlignedTEI())
      paths.map(Alignment.readCorrespondences).filter(_.nonEmpty)
    }
     else
     paths.map(Alignment.readCorrespondences).filter(_.nonEmpty)
  lazy val alignments = alignmentsOneWay.map(_.converse) ++ alignmentsOneWay // doe het toch maar weer tweezijdig
  lazy val bibles: Set[(String, String)] = alignments.flatMap(_.bibles)
}