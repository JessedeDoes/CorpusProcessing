package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import scala.xml._
object cobaltStats {

  val schemaQ = "select * from information_schema.schemata where schema_owner='dba' and not schema_name ~ 'hidden|kernel|OFF'"
  def corpusQ(schemaName: String) = s" select * from $schemaName.corpora"

  val cobalt_db_config = new database.Configuration(
    name = "gekaapte_briefjes",
    server = "svowdb16.ivdnt.loc",
    database = "cobaltje",
    user = "dba",
    password = "vercingetorix")

  val cobalt_db = new database.Database(cobalt_db_config)

  lazy val schemaInfo: Seq[Map[String, String]] = cobalt_db.slurp(cobalt_db.allRecords("information_schema.schemata"))


  /**
    * Returns a map for each corpus, with fields Set(name, url, description, corpus_engine, schemaName, full_pos_features, corpus_id)
    */

  lazy val corpora: Seq[Map[String, String]] = schemaInfo.map(m => m("schema_name")).flatMap(schemaName => {
    val table = s"$schemaName.corpora"
    lazy val corpusInfo: Seq[Map[String, String]] = try {
      cobalt_db.slurp(cobalt_db.allRecords(table)).map(m => m ++  Seq("schemaName" -> schemaName))
    } catch {
      case e => Seq()
    }
    val blacklabCorpus: Option[Map[String, String]] = corpusInfo.filter(x => x("url").contains("blacklab-server")).headOption

    blacklabCorpus
  })

  def isLeaf(n: Node): Boolean = (n.child.count(_.isInstanceOf[Text]) == n.child.size) && n.child.size <= 1
  def getCorpusInfo() = {
    corpora.map(b => {
      val url = b("url")
      val schemaName = b("schemaName")

      lazy val blsResponse = try {
        XML.load(url)
      } catch {
        case e:Exception => <bummer/>
      }

      val tokens = (blsResponse \\ "tokenCount").find(isLeaf).map(_.text.toInt).getOrElse(-1)
      val docs = (blsResponse \\ "documentCount").headOption.map(_.text.toInt).getOrElse(-1)

      println(s"${schemaName}\t$url\t$tokens\t$docs")

      b ++ Map("tokenCount" -> tokens, "documentCount" -> docs)
    })
  }

  def main(args: Array[String]): Unit = {
    getCorpusInfo()
  }
}
