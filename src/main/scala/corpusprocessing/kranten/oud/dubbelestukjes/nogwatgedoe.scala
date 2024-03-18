package corpusprocessing.kranten.oud.dubbelestukjes
import corpusprocessing.kranten.oud.Settings.krantendb
import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N
import database.DatabaseUtilities.Select
object nogwatgedoe {
  var booHoo = 0
  val showMe = "ddd:010411954:mpeg21:a0001"


  lazy val coarseArticles: Map[String,
    List[(String,Int)]] = krantendb.slurp(Select(r => (r.getString("article_id"), r.getString("article"), r.getInt("n")), "nieuw_met_overlap_distinct"))
    .groupBy(_._1).mapValues(l => l.sortBy(_._3).map(x => x._2 -> x._3))

  // select distinct id from "Krantenmetadata17eeeuwdefintieveversie1-22021nw" where "article_scannr_KB"='ddd:010411954:mpeg21:a0001';
  /*
  lazy val subheaderMapx: Map[(String, Int), Set[String]] =
    krantendb.slurp(Select(r =>
      (r.getString("article_scannr_KB"), r.getInt("id"), r.getString("subheader_int")),
      """"Krantenmetadata17eeeuwdefintieveversie1-22021nw""""))
      .groupBy(x => x._1 -> x._2.toInt)
      .mapValues(l => l.map(_._3).toSet.filter(x => x!= null && x.trim.nonEmpty))
  */

  case class SubArticle(record_id: String, kb_article_id: String, id: Int, subheader: String, text: String,
                        start_index: Option[Int]  = None,
                        matched_prefix: Option[String]  = None)
  {
    lazy val match_info = s"start_index: $start_index, matched_prefix: ${matched_prefix.getOrElse("<None>").take(10)}..."
  }


  lazy val splitArticleMap: Map[(String, Int), List[SubArticle]] =
    krantendb.slurp(Select(r =>
      SubArticle(
        r.getString("record_id"),
        r.getString("article_scannr_KB"),
        r.getInt("id"),
        r.getString("subheader_int"),
        r.getString("article_text_gesplitst")),
      """"Krantenmetadata17eeeuwdefintieveversie1-22021nw""""))
      .groupBy(x => x.kb_article_id -> x.id)
      //.mapValues(l => l.map(_._3).toSet.filter(x => x != null && x.trim.nonEmpty))




  def findLongestMatchIn(str: String, s: SubArticle): SubArticle= {
    val search = s.text.trim
    val matching: Option[(Int, Int, String)] = (search.size - 1 to 5 by -1).iterator.map(len => {
      val prefix = search.substring(0, len)
      (str.indexOf(prefix), len, prefix)
    }).find( _._1 >= 0)

    s.copy(start_index = matching.map(_._1), matched_prefix = matching.map(_._3))
    // matching
  }

   def findSubArticleStartIn(coarseArticle: String, subarticles: Seq[SubArticle]) = {

      // Console.err.println(s"looking for $eersteStukjes")
      val matched = subarticles.map(x => findLongestMatchIn(coarseArticle,x)) // .toList.sorted
      matched.sortBy(_.start_index.getOrElse(-1))
   }

  def resplitArticles(article_id: String, coarseArticles: List[(String,Int)])  = {

    println(s"\n\n###############################################################################################\n################$article_id (${coarseArticles.size})  ####################")

    coarseArticles.foreach({
      case (a,n) =>


        val subarticles = splitArticleMap(article_id -> n)


        // val resplit = articles.flatMap(a => a.replaceAll(subheaderPattern, "ZxZxZx<subheader>$1</subheader>").split("ZxZxZx"))

        val matchedSubArticles: Seq[SubArticle] = findSubArticleStartIn(a, subarticles)
        val matchable = matchedSubArticles.forall(s => s.start_index.nonEmpty)


        if (!matchable) {
          // Console.err.println("Bohhoooooooooooooooooooooooooo   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          if (subarticles.size > 1) booHoo = booHoo + 1
        }

        //val subheaders = subheaderMap(article_id -> n)
        //val a1 = if (subheaders.size > 1) a.replaceAll(subheaderPattern, "\nZxZxZx<subheader>$1</subheader>") else a // .split("ZxZxZx")) // dit werkt niet lekker
        // val subheaderPattern = "(" + subheaders.mkString("|") + ")"

        val z = a.replaceAll("<vtab/>", "\n")

        println(s"\n===================================== $n /Match$matchable: ${subarticles.size} =? ${matchedSubArticles.size} ${matchedSubArticles.map(_.start_index)} / ============= \n") // + z
        println(z)

        if (matchable) {
          val matchIndexes: Seq[Int] = 0 +: matchedSubArticles.map(_.start_index.get) :+ a.size

          val stukjes: Seq[String] = matchIndexes.indices
            .dropRight(1)
            .map(i => s"$i,${matchIndexes(i)},${matchIndexes(i+1)}:" + a.substring(matchIndexes(i), matchIndexes(i + 1)))

          subarticles.zip(stukjes).foreach({case (subarticle,stukje) => {
            println(s"//////// ${subarticle.record_id} ${subarticle.subheader} ${subarticle.text.size} ${stukje} ${subarticle.match_info}/////")
            println("___________ versie in articles_int: ____________")
            println(s"${subarticle.text.trim}")
            println("___________ versie in stukje: ____________")
            println(s"${stukje.trim}")
          }})
        }
    })
  }

  def main(args: Array[String]) : Unit = {
     coarseArticles
       .filter(_._1==showMe)
       .foreach({case (id,l) => resplitArticles(id,l)})
     //subheaderMap.foreach(println)
      println(s"Matches failed: $booHoo")
   }
}
