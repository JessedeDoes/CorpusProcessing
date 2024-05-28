package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje
import java.io.File
object Settings {
  val outputBase = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024_2/"
  val downloadDir = outputBase  + "/download/"
  val project = "courantenselectie"
  val testURL = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$project&only_validated=false"
  val directorywithCobaltExports = outputBase + "/download/"
  val trainingDataDirectory_x = outputBase + "/"  + "training-data-2/"

  lazy val trainingDataDirectory = {
    val d = new File(trainingDataDirectory_x)
    d.mkdir()
    trainingDataDirectory_x
  }

  val posTaggingDataDir = Settings.trainingDataDirectory

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
         |directorywithCobaltExports : $directorywithCobaltExports
         |lemDataDir: $lemDataDir
         |""".stripMargin)
  }
}
