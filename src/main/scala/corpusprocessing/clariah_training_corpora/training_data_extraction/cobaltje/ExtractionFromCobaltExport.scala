package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.extract_training_data_trait
import utils.zipUtils

import java.nio.file.Path

case class ExtractionFromCobaltExport(zipFileName: String, outputPrefix: String, sentenceElement: String = "s") {
  lazy val paths: Seq[Path] = zipUtils.find(zipFileName)


  case class Extractor() extends extract_training_data_trait {
    override val split_test_train_on_document_level = !outputPrefix.contains("evaluation_set")
    override lazy val output_prefix: String = outputPrefix
    override val sentence_element: String = sentenceElement
    override val enhance: Boolean = true
    // override lazy val output_folder = outputDir
  }

  def extract() = {
    val e = Extractor()
    println(e.output_folder)
    e.makeTrainingMaterialAndPartitionFromPaths(paths, e.output_prefix)
  }
}
