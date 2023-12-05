package corpusprocessing.gekaapte_brieven
import java.io.{File, PrintWriter}
import scala.xml._
import Settings._
import database.DatabaseUtilities._

import java.io
import scala.util.{Success, Try}
object exportCorpus {

  val n_to_export =  Integer.MAX_VALUE


  def cleanExportDir() = {
    val files = new java.io.File(exportDataTo).listFiles().filter(_.getName.endsWith(".xml"))
    files.foreach(f => f.delete())
  }


  val splitIntoSubsequent = false
  lazy val fieldInfo = briefdb.iterator(briefdb.allRecords("public.monster_field_info")).toList
  lazy val exportFields: Set[String] = fieldInfo.filter(x => x("exported").toLowerCase.contains("t")).map(x => x("column_name")).toSet ++ Set("xml", "brief_id")

  def init() = briefdb.runStatement(makeArticleTable)

  def exportFilter(m: Map[String, String]) = true // m("brief_id").contains("2056")

  lazy val articleGroups: List[List[Article]] =
    briefdb.iterator(briefdb.allRecords(articleTable))
      .filter(exportFilter)
      .map(x => x.filter(y => exportFields.contains(y._1)))
      .map(x => Article(x))
      .toList
      .groupBy(_.id)
      .values
      .toList


  lazy val articles = articleGroups.map(a => Article.groupArticles(a))

  lazy val groupMetaMap: Map[String, List[Metadata]] = articles.map(_.metadata).groupBy(x => x("groepID_INT")).mapValues(Metadata.groupMetadata).mapValues(List(_))

  lazy val splitGroupsIntoContinuousSequences: Map[String, Iterable[Metadata]] = {
    val groups1 = articles.map(_.metadata).groupBy(x => x("groepID_INT")).values
    val groups2 = groups1.flatMap(g => Metadata.splitIntoSequences(g)).map(Metadata.groupMetadata)
    groups2.groupBy(m => m("groepID_INT"))
  }

  lazy val groupingToUse = if (splitIntoSubsequent) splitGroupsIntoContinuousSequences else groupMetaMap
  lazy val articlesWithGroupMetadata: Seq[Article] = articles.filter(a => a.metadata.contains("groepID_INT"))
    .map(a => a.copy(groupMetadata = groupingToUse(a -> "groepID_INT").filter(gm => gm.groupMemberIds.contains(a.id)).headOption))

  def main(args: Array[String])  = {

    init()
    println(exportFields)

    // articlesWithGroupMetadata.foreach(a => a.metadata.report())

    cleanExportDir()
    val extra = articlesWithGroupMetadata.filter(a =>  (a -> "groepID_INT").contains("1270"))  // http://svotmc10.ivdnt.loc:8080/corpus-frontend/gekaapte_brieven/docs/nl-hana_hca30-749_9_4_0015
    val toDo = (articlesWithGroupMetadata.take(n_to_export) ++ extra).toSet.filter(x => !x.textMissing)
    toDo.foreach(x => {
        println("Exporting:" + x.fields("brief_id"))
        XML.save(exportDataTo + x.fields("brief_id") + ".xml", x.prettyXML)
      })
  }
}


