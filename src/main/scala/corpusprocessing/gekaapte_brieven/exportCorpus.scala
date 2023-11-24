package corpusprocessing.gekaapte_brieven
import java.io.PrintWriter
import scala.xml._
import Settings._
import database.DatabaseUtilities._
import scala.util.{Success, Try}
object exportCorpus {

  val n_to_export = Integer.MAX_VALUE

  def main(args: Array[String])  = {
    briefdb.runStatement(makeArticleTable)

    val articleGroups: List[List[Article]] =
      briefdb.iterator(briefdb.allRecords(articleTable))
      .map(x => Article(x))
      .take(n_to_export)
      .toList
      .groupBy(_.id)
      .values
      .toList

    val groupedArticles = articleGroups.map(a => Article.groupArticles(a))


    groupedArticles.foreach(x => {
        XML.save(exportDataTo + x.fields("brief_id") + ".xml", x.prettyXML)
        println(x.fields("brief_id"))
      })
  }
}
