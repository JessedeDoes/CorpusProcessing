package corpusprocessing.gekaapte_brieven
import java.io.PrintWriter
import scala.xml._
import Settings._
import database.DatabaseUtilities._
import scala.util.{Success, Try}
object exportCorpus {

  val n_to_export = Integer.MAX_VALUE

  def main(args: Array[String])  = {
    lazy val fieldInfo = briefdb.iterator(briefdb.allRecords("public.monster_field_info")).toList
    lazy val exportFields: Set[String] = fieldInfo.filter(x => x("exported").toLowerCase.contains("t")).map(x => x("column_name")).toSet ++ Set("xml", "brief_id")
    println(exportFields)

    briefdb.runStatement(makeArticleTable)



    
    val articleGroups: List[List[Article]] =
      briefdb.iterator(briefdb.allRecords(articleTable))
        .map(x => x.filter(y => exportFields.contains(y._1)))
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
