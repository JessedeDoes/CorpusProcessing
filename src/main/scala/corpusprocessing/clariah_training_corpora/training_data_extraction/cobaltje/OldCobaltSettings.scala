package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje
import java.io.File

case class CobaltExportSettings() {

  val outputBase = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/" // "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/"
  val downloadDir = outputBase  + "/download/"
  val cobaltServeExport = "http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/"
  //val project = "courantenselectie"
  //val testURL = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$project&only_validated=false"


  val cobalt_db_config = new database.Configuration(
    name = "gekaapte_briefjes",
    server = "svowdb16.ivdnt.loc",
    database = "cobaltje",
    user = "dba",
    password = "vercingetorix")

  val blacklab_server = "?"

  val directoryWithCobaltExports = outputBase + "/download/"
  val trainingDataDirectory = outputBase + "/"  + "training-data-2/"

  lazy val createdTrainingDataDirectory = {
    val d = new File(trainingDataDirectory)
    d.mkdir()
    trainingDataDirectory
  }

  lazy val posTaggingDataDir = createdTrainingDataDirectory

  lazy val lemDataDir = {
    val parent = new File(posTaggingDataDir).getParent
    val d = outputBase + "/Lemmatizer/";
    new File(d).mkdir();
    d
  }

  def main(args: Array[String]) = {
    println(
      s"""
         |pos : $posTaggingDataDir
         |directorywithCobaltExports : $directoryWithCobaltExports
         |lemDataDir: $lemDataDir
         |""".stripMargin)
  }
}

object OldCobaltSettings extends CobaltExportSettings()

// psql -h svprll01.ivdnt.loc -U lancelot -d lancelot
object LancelotSettings extends CobaltExportSettings() {
  // http://lancelot.ivdnt.loc/CobaltServe/webservice/api/
  override  val cobaltServeExport = "http://lancelot.ivdnt.loc/CobaltServe/webservice/api/export/"
  // http://lancelot.ivdnt.loc/lancelot/search/lancelot:19e-eeuwse_kranten_DEFINITIEF/search/
  override val blacklab_server: String = "http://lancelot.ivdnt.loc/lancelot/search/" //
  override val cobalt_db_config = new database.Configuration(
    name = "lancelotje",
    server = "svprll01.ivdnt.loc",
    database = "lancelot",
    user = "lancelot",
    password = "daargingeenriddertepaard")
}
