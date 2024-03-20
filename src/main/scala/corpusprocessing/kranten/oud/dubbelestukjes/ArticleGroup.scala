package corpusprocessing.kranten.oud.dubbelestukjes

case class ArticleGroup(kb_article_id: String, articles: List[String], record_ids: List[String], subheaderz: List[String]) {

  def n_grams(s: String, n: Int): Set[List[String]] = s.split("\\s+").toList.sliding(n).toSet

  val arts: Seq[(String, String)] = record_ids.zip(articles)
  val artMap = arts.toMap
  val subheaders = record_ids.zip(subheaderz).toMap

  def overlaps_0(n: Int): Seq[(String, Seq[(String, Set[List[String]])])] = {
    arts.map(a => {
      val g = n_grams(a._2, n)
      val overlappies: Seq[(String, Set[List[String]])] =
        arts
          .filter(_._1 != a._1)
          .map({ case (id1, s1) => id1 -> n_grams(s1, n).intersect(g)
          }).filter(_._2.nonEmpty)
      a._1 -> overlappies
    }
    ).filter(_._2.nonEmpty)
  }

  def overlaps(n: Int): Set[Overlap] = overlaps_0(n).flatMap({ case (id, l) => l.map({ case (id1, s) => Overlap(this, kb_article_id, id, id1, s) }) }).toSet.filter(o => o.id2 > o.id1)
}
