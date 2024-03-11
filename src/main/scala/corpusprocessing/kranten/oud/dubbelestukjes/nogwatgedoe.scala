package corpusprocessing.kranten.oud.dubbelestukjes
import corpusprocessing.kranten.oud.Settings.krantendb
import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N
import database.DatabaseUtilities.Select
object nogwatgedoe {
  lazy val arts: Map[String, List[String]] = krantendb.slurp(Select(r => (r.getString("article_id"), r.getString("article"), r.getInt("n")), "oud_met_overlap_distinct")).groupBy(_._1).mapValues(l => l.sortBy(_._3).map(_._2))
  lazy val subheaderMap: Map[String, Set[String]] = krantendb.slurp(Select(r => (r.getString("article_id"), r.getString("subheader")), "oud_met_overlap")).groupBy(_._1).mapValues(l => l.map(_._2).toSet.filter(_.trim.nonEmpty))

  def resplitArticles(article_id: String, articles: List[String])  = {
    val subheaders = subheaderMap(article_id)
    val subheaderPattern = "(" + subheaders.mkString("|")  + ")"
    val resplit = articles.flatMap(a => a.replaceAll(subheaderPattern, "ZxZxZx<subheader>$1</subheader>").split("ZxZxZx"))
    println(s"################$article_id ==> $subheaderPattern ####################")
    articles.foreach(println)
  }
  def main(args: Array[String]) : Unit = {
     arts.foreach({case (id,l) => resplitArticles(id,l)})
     //subheaderMap.foreach(println)
   }
}
