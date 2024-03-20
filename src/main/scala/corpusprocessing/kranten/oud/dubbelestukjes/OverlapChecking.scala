package corpusprocessing.kranten.oud.dubbelestukjes

import corpusprocessing.kranten.oud.Settings.krantendb
import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N
import database.DatabaseUtilities.Select
import utils.alignment.{AlignmentGeneric, SimOrDiff}

import java.io.PrintWriter
import java.util.Comparator
object comp extends Comparator[(String,String)]
{
  import java.text.Normalizer

  def flatten(string : String) = Normalizer.normalize(string, Normalizer.Form.NFD).replaceAll("\\p{M}", "").replaceAll("[|*\\]\\[]","");

  override def compare(t: (String,String), t1: (String,String)):Int =
  {
    flatten(t._2).compareToIgnoreCase(flatten(t1._2))
  }
}


import corpusprocessing.kranten.oud.dubbelestukjes.eenPaarQueries._

object OverlapChecking {

  val N = 10
  val addOriginalText = true
  val log_overlaps = false

  lazy val qo = Select(r => ArticleGroup(r.getString("kb_article_id"), r.getStringArray("articles").toList, r.getStringArray("record_ids").toList, r.getStringArray("subheaders").toList), "groupie" )

  lazy val overlap_check = krantendb.iterator(qo)

  implicit def s2b(p: (String, String)): krantendb.Binding = krantendb.Binding(p._1, p._2)
  implicit def itb(p: (String, Int)): krantendb.Binding = krantendb.Binding(p._1, p._2)

  val batchInsertion = krantendb.QueryBatch[Overlap](
    s"""insert into ${overlap_base_name}_$N
       |(kb_article_id, id1, id2, n, example, text1, text2, matchOrder)
       |values (:kb_article_id, :id1, :id2, :n, :example, :text1, :text2, :matchOrder)""".stripMargin,

    o => Seq(
      "kb_article_id" -> o.kb_article_id,
      "id1" -> o.id1,
      "id2" -> o.id2,
      "n" -> o.evidence.size.toString,
      "example" -> o.evidence.toList.sortBy(x => x.toString).head.mkString(" "),
      "text1" -> o.art1_aligned,
      "text2" -> o.art2_aligned,
      "matchOrder" -> o.positionOf(o.id2)
    )
  )

  def logOverlaps(all_overlaps: Stream[Set[Overlap]]) = {
    val log = new PrintWriter("/tmp/couranten_overlaps.txt")
    all_overlaps.foreach(o => {
      println(s"Ouch ${o.head.kb_article_id}!")
      log.println(s"\n#####  ${o.head.kb_article_id} #####")
      o.foreach(x => log.println("\t" + x))
      o.foreach(_.align())
    })
    log.close()
  }

  def count(S: Set[Overlap], id: String) = S.count(o => o.id1 == id || o.id2 == id)

  def size(S: Set[Overlap], id: String) = S.filter(_.id1 == id).map(_.art1).union(S.filter(_.id2 == id).map(_.art2)).headOption.map(_.length).getOrElse(0)
  def identifyCulprits(S: Set[Overlap]): Set[String] = {

    val ids = S.flatMap(o => Set(o.id1,o.id2))
    val multiple  = ids.filter(count(S,_) > 1)
    if (false && multiple.nonEmpty) multiple else Set(ids.toList.maxBy(size(S, _)))
  }

  def postProcessOverlaps(S: Set[Overlap]): Set[Overlap] = {
    val culprits = identifyCulprits(S)
    def maybeSwap(o: Overlap)  = {
      if (culprits.contains(o.id2) && !culprits.contains(o.id1) || size(S,o.id2)  > size(S,o.id1)) o.swap() else o
    }
    val z = S.filter(o => culprits.contains(o.id1)).groupBy(_.id1).mapValues(g => {
      val allRelated: List[(String, String)] = g.map(o => o.id2 -> o.art2).toSet.toList
      g.map(_.copy(allRelated = allRelated))
    }).values.flatten.toSet
    z
  }

  def main(args: Array[String]) = {

    krantendb.runStatement(drop_overlap_table)
    krantendb.runStatement(check_article_overlap_query)
    krantendb.runStatement(create_overlap_table)

    lazy val all_overlaps: Stream[Set[Overlap]] = overlap_check.map(g => g.overlaps(N)).filter(_.nonEmpty).toStream
    lazy val filtered_overlaps = all_overlaps.map(s => postProcessOverlaps(s))

    batchInsertion.insert(filtered_overlaps.flatten)
    krantendb.runStatement(create_grouped_view)

    if (addOriginalText) {
      krantendb.runStatement(addtext1)
      krantendb.runStatement(addtext2)
    }

    if (log_overlaps)
      logOverlaps(filtered_overlaps)

    println(create_grouped_view)
  }
}

/*
create materialized view length_comparison as select "article_scannr_KB", max(length("article_text_CS")) as length_kb, sum(length(article_text_int)) as length_sum, sum(1) as n_articles from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB";

create view grouping_by_article_id_plus_pagina as select "article_scannr_KB", "article_scannr_KB_pluspagina", max("article_text_CS") as txt from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB", "article_scannr_KB_pluspagina";

create materialized view kb_article_fulltext as select
 "article_scannr_KB", string_agg(txt, '<hr>' order by  "article_scannr_KB_pluspagina") as text  from grouping_by_article_id_plus_pagina group by "article_scannr_KB"
 ;

*/