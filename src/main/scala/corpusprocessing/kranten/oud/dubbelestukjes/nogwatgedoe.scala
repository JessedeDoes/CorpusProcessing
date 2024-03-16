package corpusprocessing.kranten.oud.dubbelestukjes
import corpusprocessing.kranten.oud.Settings.krantendb
import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N
import database.DatabaseUtilities.Select
object nogwatgedoe {
  val showMe = "ddd:010411954:mpeg21:a0001"
  lazy val arts: Map[String, List[(String,Int)]] = krantendb.slurp(Select(r => (r.getString("article_id"), r.getString("article"), r.getInt("n")), "oud_met_overlap_distinct"))
    .groupBy(_._1).mapValues(l => l.sortBy(_._3).map(x => x._2 -> x._3))

  // select distinct id from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" where "article_scannr_KB"='ddd:010411954:mpeg21:a0001';
  lazy val subheaderMap: Map[(String, Int), Set[String]] =
    krantendb.slurp(Select(r =>
      (r.getString("article_scannr_KB"), r.getInt("id"), r.getString("subheader_int")),
      """"Krantenmetadata17eeeuwdefintieveversie1-22021nw""""))
      .groupBy(x => x._1 -> x._2.toInt)
      .mapValues(l => l.map(_._3).toSet.filter(x => x!= null && x.trim.nonEmpty))

  lazy val subheaderMapy: Map[(String,Int), Set[String]] =
    krantendb.slurp(Select(r => (r.getString("article_id"), r.getString("n1"), r.getString("subheader")), "oud_met_overlap")).groupBy(x => x._1 -> x._2.toInt).mapValues(l => l.map(_._3).toSet.filter(_.trim.nonEmpty))

  def resplitArticles(article_id: String, articles: List[(String,Int)])  = {

    println(s"################$article_id (${articles.size})  ####################")

    articles.foreach({
      case (a,n) =>
        val subheaders = subheaderMap(article_id -> n)
        val subheaderPattern = "(" + subheaders.mkString("|") + ")"
        // val resplit = articles.flatMap(a => a.replaceAll(subheaderPattern, "ZxZxZx<subheader>$1</subheader>").split("ZxZxZx"))
        val a1 = if (subheaders.size > 1) a.replaceAll(subheaderPattern, "\nZxZxZx<subheader>$1</subheader>") else a // .split("ZxZxZx"))
        println(s"## $n  (${subheaders.size}) $subheaderPattern ## \n" + a1.replaceAll("<vtab/>", "\n"))
    })
  }
  def main(args: Array[String]) : Unit = {
     arts
       .filter(_._1==showMe)
       .foreach({case (id,l) => resplitArticles(id,l)})
     //subheaderMap.foreach(println)
   }
}
