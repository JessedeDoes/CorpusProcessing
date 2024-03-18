package corpusprocessing.kranten.oud.dubbelestukjes
import corpusprocessing.kranten.oud.Settings.krantendb
import corpusprocessing.kranten.oud.dubbelestukjes.OverlapChecking.N
import database.DatabaseUtilities.Select

import scala.collection.immutable
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
                        end_index: Option[Int]  = None,
                        matched_prefix: Option[String]  = None)
  {
    lazy val match_info = s"start_index: $start_index, end_index: $end_index matched_prefix: ${matched_prefix.getOrElse("<None>").take(10)}..."
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
      val r: Seq[SubArticle] = matched.sortBy(_.start_index.getOrElse(-1))
      val z = r.indices.map(i => {
        val e = if (i < r.size-1) r(i+1).start_index else Some(coarseArticle.size)
        r(i).copy(end_index = e)
      })
      z.foreach(x => Console.err.println(x.match_info))
      z
   }

  def resplitArticles(article_id: String, coarseArticles: List[(String,Int)]): Seq[SubArticle] = {

    println(s"\n\n###############################################################################################\n################$article_id (${coarseArticles.size})  ####################")

    coarseArticles.flatMap({
      case (a,n) =>

        val subarticles = splitArticleMap(article_id -> n)

        // val resplit = articles.flatMap(a => a.replaceAll(subheaderPattern, "ZxZxZx<subheader>$1</subheader>").split("ZxZxZx"))

        val matchedSubArticles: Seq[SubArticle] = findSubArticleStartIn(a, subarticles)
        val matchable = matchedSubArticles.forall(s => s.start_index.nonEmpty && s.end_index.nonEmpty)

        if (!matchable) {
          if (subarticles.size > 1) booHoo = booHoo + 1
        }

        //val subheaders = subheaderMap(article_id -> n)
        //val a1 = if (subheaders.size > 1) a.replaceAll(subheaderPattern, "\nZxZxZx<subheader>$1</subheader>") else a // .split("ZxZxZx")) // dit werkt niet lekker
        // val subheaderPattern = "(" + subheaders.mkString("|") + ")"

        val z = a.replaceAll("<vtab/>", "\n")

        println(s"\n===================================== $n /Match$matchable: ${subarticles.size} =? ${matchedSubArticles.size} ${matchedSubArticles.map(_.start_index)} / ============= \n") // + z

        if (matchable) {
          matchedSubArticles.map({subarticle => {
            val stukje = a.substring(subarticle.start_index.get, subarticle.end_index.get)

            if (stukje.trim.size < subarticle.text.trim.size - 50) {
              println(s"~~~~~~~~~~~~~~~~~~ ${subarticle.record_id} ${subarticle.subheader} ${subarticle.text.size}  ${subarticle.match_info} ~~~~~~~~~~ ")
              println("___________ versie in articles_int: ____________")
              println(s"${subarticle.text.trim}")
              println("___________ versie in stukje: ____________")
              println(s"${stukje.trim}")
            }
            subarticle.copy(text=stukje)
          }})
        } else List()
    })
  }

  def insertChoppedArticles(subarticles: Seq[SubArticle]) = {
    implicit def xtb(p: (String, String)): krantendb.Binding = krantendb.Binding(p._1, p._2)
    krantendb.runStatement("create table if not exists articles_resplit (record_id text, content text)")
    val batchInsertion = krantendb.QueryBatch[SubArticle](s"insert into articles_resplit (record_id, content) values (:record_id, :content)",
      o => Seq(
        "record_id" -> o.record_id,
        "content" -> o.text,
      )
    )
    batchInsertion.insert(subarticles)
  }

  def main(args: Array[String]) : Unit = {
     val adaptedSubArticles = coarseArticles
       //.filter(_._1==showMe)
       .flatMap({case (id,l) => resplitArticles(id,l)}).toSeq
      insertChoppedArticles(adaptedSubArticles)
      println(s"Matches failed: $booHoo")
   }
}
