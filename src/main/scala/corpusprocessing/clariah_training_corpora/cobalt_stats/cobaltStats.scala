package corpusprocessing.clariah_training_corpora.cobalt_stats
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

  lazy val schemaInfo = cobalt_db.slurp(cobalt_db.allRecords("information_schema.schemata"))

  def main(args: Array[String])  = {
    schemaInfo.map(m => m("schema_name")).foreach(schemaName => {
      val table = s"$schemaName.corpora"
      lazy val corpusInfo = try { cobalt_db.slurp(cobalt_db.allRecords(table)) } catch {
        case e => Seq()
      }
      val blacklabCorpus = corpusInfo.filter(x => x("url").contains("blacklab-server")).headOption
      blacklabCorpus.foreach(b => {
        val url = b("url")
        // println(s"$schemaName=>$url")
        lazy val blsResponse = try {XML.load(url) } catch {
          case e => <bummer/>
        }

        val tokens = (blsResponse \\ "tokenCount").headOption.map(_.text.toInt).getOrElse(-1)
        val docs = (blsResponse \\ "documentCount").headOption.map(_.text.toInt).getOrElse(-1)
        println(s"$schemaName\t$url\t$tokens\t$docs")
      })
    })
  }
}
