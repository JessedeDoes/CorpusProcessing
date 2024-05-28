package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import java.io.File

import Settings._

object ExtractionFromCobaltTrainingCorpora {

  def main(args: Array[String]) = {

    new File(directorywithCobaltExports).listFiles()
      .filter(_.getName.endsWith(".zip"))
      //.filter(_.getName.contains("gtbcit_mnw_15"))
      .foreach(f => {
        val outputFilenamePrefix = f.getName().replaceAll("^cobalt_export_", "").replaceAll(".zip", "")
        val dirAsDir = new File(trainingDataDirectory + "/" + outputFilenamePrefix)
        // dirAsDir.delete()
        dirAsDir.mkdir()
        val outputPrefix = trainingDataDirectory + "/" + outputFilenamePrefix + "/" + outputFilenamePrefix
        val e = ExtractionFromCobaltExport(f.getCanonicalPath, outputPrefix, sentenceElement = if (outputFilenamePrefix.contains("cit")) "q" else "s")
        e.extract()
      })
  }
}
