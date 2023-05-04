package corpusprocessing.kranten.oud

import corpusprocessing.kranten.oud.OverlapChecking.N
import corpusprocessing.kranten.oud.Settings.krantendb
import database.DatabaseUtilities.Select

import java.io.PrintWriter
import scala.collection.JavaConverters._
import utils.alignment.{AlignmentGeneric, SimOrDiff}

import scala.xml._
import java.io.File
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

case class Overlap(g: Groepje, kb_article_id: String, id1: String, id2: String, evidence: Set[List[String]]) {
  override def toString() = s"$kb_article_id:($id1 $id2) [${evidence.size}] ${evidence.toList.sortBy(x => x.toString).head.mkString(" ")}"
  lazy val art1 = g.artMap(id1)
  lazy val art2 = g.artMap(id2)

  lazy val subheader1 = g.subheaders(id1)
  lazy val subheader2 = g.subheaders(id2)

  def align() = {
    def z(x: List[String]) = x.flatMap(_.split("nnnxnxnxnxnxnxn")).zipWithIndex.map({case (y,i) => (i.toString,y)})
    val a = new AlignmentGeneric(comp) // dit werkt niet...

    val z1 = z(art1.split("\\s+").toList)
    val z2 = z(art2.split("\\s+").toList)

    val chunks: Seq[SimOrDiff[(String, String)]] = a.findChunks(z1, z2)

    val lr: Seq[(Boolean, List[(String, String)], List[(String, String)], Int)] = chunks.map(
      c => {
        //Console.err.println(c)
        val left = z1.slice(c.leftStart, c.leftEnd)
        val right = z2.slice(c.rightStart, c.rightEnd)
        (c.isSimilarity, left, right, c.leftStart)
      })

    val ltext = lr.map(c => {
      val l = c._2.map(_._2).mkString(" ")
       if (c._1) "<b>" + l + "</b>" else l
    })

    val rtext = lr.map(c => {
      val l = c._3.map(_._2).mkString(" ")
      if (c._1) "<b>" + l + "</b>" else l
    })

    (ltext, rtext)
  }

  lazy val alignment = align()
  lazy val art1_aligned = s"<h2>$subheader1</h2>" + alignment._1.mkString(" ")
  lazy val art2_aligned = s"<h2>$subheader2</h2>" + alignment._2.mkString(" ")
  // hier nog een difje aan toevoegen .......
}

case class Groepje(kb_article_id: String, articles: List[String], record_ids: List[String], subheaderz: List[String]) {

  def n_grams(s: String, n: Int): Set[List[String]] = s.split("\\s+").toList.sliding(n).toSet

  val arts: Seq[(String, String)] = record_ids.zip(articles)
  val artMap = arts.toMap
  val subheaders = record_ids.zip(subheaderz).toMap

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

  def overlaps(n: Int): Set[Overlap] = overlaps_0(n).flatMap({case (id, l)  => l.map({case (id1, s) => Overlap(this, kb_article_id, id, id1, s) })}).toSet.filter(o => o.id2 > o.id1)
}


object eenPaarQueries  {
  val check_article_overlap_query =
    """create temporary view groupie as
     select kb_article_id,
     array_agg(article_text order by record_id) as articles,
     array_agg(record_id order by record_id) as record_ids,
     array_agg(subheader_int order by record_id) as subheaders
     from articles_int
     where kb_issue in (select kb_issue from issues_kb_fixed where not dubbel_mag_weg)
     group by kb_article_id;"""

  val drop_overlap_table = s"drop table overlappings_$N"
  val create_overlap_table = s"create table overlappings_$N (kb_article_id text, id1 text, id2 text, n text, example text, text1 text, text2 text)"
  val addtext1 = s"update overlappings_$N set text1 = articles_int.article_text from articles_int where cast(record_id as text) = id1"
  val addtext2 = s"update overlappings_$N set text2 = articles_int.article_text from articles_int where cast(record_id as text) = id2"
}

import eenPaarQueries._

object OverlapChecking {

  val N = 6


  lazy val qo = Select(r => Groepje(r.getString("kb_article_id"), r.getStringArray("articles").toList, r.getStringArray("record_ids").toList, r.getStringArray("subheaders").toList), "groupie" )

  lazy val overlap_check = krantendb.iterator(qo)

  implicit def xtb(p: (String, String)): krantendb.Binding = krantendb.Binding(p._1, p._2)

  val b = krantendb.QueryBatch[Overlap](s"insert into overlappings_$N (kb_article_id, id1, id2, n, example, text1, text2) values (:kb_article_id, :id1, :id2, :n, :example, :text1, :text2)",
    o => Seq(
      "kb_article_id" -> o.kb_article_id,
      "id1" -> o.id1,
      "id2" -> o.id2,
      "n" -> o.evidence.size.toString,
      "example" -> o.evidence.toList.sortBy(x => x.toString).head.mkString(" "),
      "text1" -> o.art1_aligned,
      "text2" -> o.art2_aligned
    )
  )

  val log_overlaps = false

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

  def main(args: Array[String]) = {

    krantendb.runStatement(drop_overlap_table)
    krantendb.runStatement(check_article_overlap_query)
    krantendb.runStatement(create_overlap_table)


    val all_overlaps: Stream[Set[Overlap]] = overlap_check.map(g => g.overlaps(N)).filter(_.nonEmpty).toStream

    b.insert(all_overlaps.flatMap(identity))


    if (log_overlaps)
      logOverlaps(all_overlaps)

    //krantendb.runStatement(addtext1)
    //krantendb.runStatement(addtext2)

  }
}

/*
create materialized view length_comparison as select "article_scannr_KB", max(length("article_text_CS")) as length_kb, sum(length(article_text_int)) as length_sum, sum(1) as n_articles from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB";


create view grouping_by_article_id_plus_pagina as select "article_scannr_KB", "article_scannr_KB_pluspagina", max("article_text_CS") as txt from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" group by "article_scannr_KB", "article_scannr_KB_pluspagina";

create materialized view kb_article_fulltext as select
 "article_scannr_KB", string_agg(txt, '<hr>' order by  "article_scannr_KB_pluspagina") as text  from grouping_by_article_id_plus_pagina group by "article_scannr_KB"
 ;

 */