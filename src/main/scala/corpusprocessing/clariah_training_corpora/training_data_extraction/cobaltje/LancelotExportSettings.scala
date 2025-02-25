package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje
import java.io.File
object Rename {
  val renaming: Map[String,String] =Map(
    "evaluation_set_15" -> "dbnl-excerpts-15",
    "evaluation_set_16" -> "dbnl-excerpts-16",
    "evaluation_set_17" -> "dbnl-excerpts-17",
    "evaluation_set_18" -> "dbnl-excerpts-18",
    "evaluation_set_19" -> "dbnl-excerpts-19",
    "clvn_selectie_met_wat_minder_duits" -> "clvn",
    "courantenselectie" -> "couranten",
    "gtbcit_14_fromscratch" -> "dictionary-quotations-14",
    "gtbcit_mnw_15" -> "dictionary-quotations-15",
    "gtbcit_punct_16" -> "dictionary-quotations-16",
    "gtbcit_punct_17" -> "dictionary-quotations-17",
    "wnt_citaten_18" -> "dictionary-quotations-18",
    "wnt_citaten_19" -> "dictionary-quotations-19",
    "bab_enhanced_hoofdlettertest" -> "letters-as-loot",
    "eee1729763457532" -> "kranten-19",
    "gys1732186374071" -> "gysseling-ambtelijk",
    "gysseling_literair_selectie_tdn_enhance" -> "gysseling-literair"
  )
}

case class LancelotExportSettings() {

  val outputBase = "/data/Lancelot/" // /mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/" // "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/"
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



// psql -h svprll01.ivdnt.loc -U lancelot -d lancelot
object LancelotSettings extends LancelotExportSettings() {
  // http://lancelot.ivdnt.loc/CobaltServe/webservice/api/
  override  val cobaltServeExport = "http://172.16.4.31/CobaltServe/webservice/api/export/" // lancelot.ivdnt.loc
  // http://lancelot.ivdnt.loc/lancelot/search/lancelot:19e-eeuwse_kranten_DEFINITIEF/search/
  override val blacklab_server: String = "http://lancelot.ivdnt.loc/lancelot/search/" //
  override val cobalt_db_config = new database.Configuration(
    name = "lancelotje",
    server = "svowdb20.ivdnt.loc", // 172.16.4.31", // svprll01.ivdnt.loc (172.16.4.31):
    database = "lancelot",
    user = "postgres", // "lancelot",
    password = "inl"
     ) // "daargingeenriddertepaard")
}

object OldCobaltSettings extends LancelotExportSettings()