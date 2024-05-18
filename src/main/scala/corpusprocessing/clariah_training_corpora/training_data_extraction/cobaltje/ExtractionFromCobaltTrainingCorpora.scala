package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import java.io.File

object ExtractionFromCobaltTrainingCorpora {
  val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024/"
  def main(args: Array[String]) = {

    new File(dir).listFiles()
      .filter(_.getName.endsWith(".zip"))
      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .foreach(f => {
        val outputFilenamePrefix = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        new File(dir + "/" + outputFilenamePrefix).mkdir()
        val outputPrefix = dir + "/" + outputFilenamePrefix + "/" + outputFilenamePrefix
        val e = ExtractorFromZippie(f.getCanonicalPath, outputPrefix, sentenceElement = if (outputFilenamePrefix.contains("cit")) "q" else "s")
        e.extract()
      })
  }
}
