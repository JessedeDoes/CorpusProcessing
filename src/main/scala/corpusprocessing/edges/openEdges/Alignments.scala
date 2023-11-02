package corpusprocessing.edges.openEdges

import corpusprocessing.edges.openEdges.Verse.verse

case class Alignment(v1: Set[VerseRef], v2: Set[VerseRef])
object Alignments {
  def readAlignments(path: String) = {

    val name = new java.io.File(path).getName
    val parts = name.split("-")
    val b1 = parts(0).replaceAll("\\.tsv", "")
    val b2 = parts(1).replaceAll("\\.tsv", "")

    val alignments = io.Source.fromFile(path).getLines().map(l => {
      val parts = l.split("\\t")
      val verses_1 = parts(0).split(",").map(x => verse(b1, x)).toSet
      val verses_2 = parts(1).split(",").map(x => verse(b2, x)).toSet
      Alignment(verses_1, verses_2)
    })

    Alignments(alignments.toSet)
  }

}

case class Alignments(alignments: Set[Alignment]) {
  lazy val index1: Map[String, Set[Alignment]] = alignments.flatMap(a => a.v1.map(r => r.xmlId -> a)).groupBy(_._1).mapValues(_.map(_._2))
  lazy val index2: Map[String, Set[Alignment]] = alignments.flatMap(a => a.v2.map(r => r.xmlId -> a)).groupBy(_._1).mapValues(_.map(_._2))

  def aligned(v: VerseRef): Set[VerseRef] = {
    val id = v.xmlId
    val a1 = index1.getOrElse(id, Set[Alignment]())
    val a2 = index2.getOrElse(id, Set[Alignment]())
    if (a1.nonEmpty) {
      a1.flatMap(_.v2)
    } else a2.flatMap(_.v1)
  }
}