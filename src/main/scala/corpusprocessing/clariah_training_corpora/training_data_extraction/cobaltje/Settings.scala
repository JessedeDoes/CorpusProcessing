package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje
import java.io.File

case class CobaltExportSettings() {

  val outputBase = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/"
  val downloadDir = outputBase  + "/download/"
  val cobaltServeExport = "http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/"
  //val project = "courantenselectie"
  //val testURL = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$project&only_validated=false"

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

object Settings extends CobaltExportSettings()
