package corpusprocessing.kranten.oud

import corpusprocessing.kranten.oud.Settings.krantendb
import database.DatabaseUtilities.Select
import java.io.PrintWriter

import scala.collection.JavaConverters._

case class Overlap(kb_article_id: String, id1: String, id2: String, evidence: Set[List[String]]) {
  override def toString() = s"$kb_article_id:($id1 $id2) [${evidence.size}] ${evidence.toList.sortBy(x => x.toString).head.mkString(" ")}"
}

case class Groepje(kb_article_id: String, articles: List[String], record_ids: List[String]) {

  def n_grams(s: String, n: Int): Set[List[String]] = s.split("\\s+").toList.sliding(n).toSet

  val arts: Seq[(String, String)] = record_ids.zip(articles)

  def overlaps_0(n: Int): Seq[(String, Seq[(String, Set[List[String]])])] = {
    arts.map(a => {
       val g = n_grams(a._2, n)
       val overlappies: Seq[(String, Set[List[String]])] = arts.filter(_._1 != a._1).map({case (id1, s1) =>
          id1 -> n_grams(s1,n).intersect(g)
       }).filter(_._2.nonEmpty)
      a._1 -> overlappies
     }
    ).filter(_._2.nonEmpty)
  }

  def overlaps(n: Int): Set[Overlap] = overlaps_0(n).flatMap({case (id, l)  => l.map({case (id1, s) => Overlap(kb_article_id, id,id1, s) })}).toSet.filter(o => o.id2 > o.id1)
}


object OverlapChecking {

  val check_article_overlap_query = "create temporary view groupie as select kb_article_id, array_agg(article_text order by record_id) as articles, array_agg(record_id order by record_id) as " +
    "record_ids from articles_int group by kb_article_id;"

  val drop_overlap_table = "drop table overlappings"
  val create_overlap_table = "create table overlappings (kb_article_id text, id1 text, id2 text, n text, example text, text1 text, text2 text)"
  val addtext1 = "update overlappings set text1 = articles_int.article_text from articles_int where cast(record_id as text) = id1"
  val addtext2 = "update overlappings set text2 = articles_int.article_text from articles_int where cast(record_id as text) = id2"
  lazy val qo = Select(r => Groepje(r.getString("kb_article_id"), r.getStringArray("articles").toList, r.getStringArray("record_ids").toList), "groupie" )

  lazy val overlap_check = krantendb.iterator(qo)

  implicit def xtb(p: (String, String)): krantendb.Binding = krantendb.Binding(p._1, p._2)

  val b = krantendb.QueryBatch[Overlap]("insert into overlappings (kb_article_id, id1, id2, n, example) values (:kb_article_id, :id1, :id2, :n, :example)",
    o => Seq(
      "kb_article_id" -> o.kb_article_id,
      "id1" -> o.id1,
      "id2" -> o.id2,
      "n" -> o.evidence.size.toString,
      "example" -> o.evidence.toList.sortBy(x => x.toString).head.mkString(" ")
    )
  )

  val N = 8
  def main(args: Array[String]) = {

    krantendb.runStatement(drop_overlap_table)
    krantendb.runStatement(check_article_overlap_query)
    krantendb.runStatement(create_overlap_table)
    val log = new PrintWriter("/tmp/couranten_overlaps.txt")

    val all_overlaps = overlap_check.map(g => g.overlaps(N)).filter(_.nonEmpty)
    b.insert(all_overlaps.toStream.flatMap(identity))

    all_overlaps.foreach(o => {
        println(s"Ouch ${o.head.kb_article_id}!")
        log.println(s"\n#####  ${o.head.kb_article_id} #####")
        o.foreach(x => log.println("\t" + x))
    })

    krantendb.runStatement(addtext1)
    krantendb.runStatement(addtext2)
    log.close()
  }
}

/*
create materialized view length_comparison as select "article_scannr_KB", max(length("article_text_CS")) as length_kb, sum(length(article_text_int)) as length_sum, sum(1) as n_articles from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB";
 */