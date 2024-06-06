package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.{Partition, TrainingDataExtraction, TrainingDataInfo}
import utils.zipUtils

import java.nio.file.Path

case class ExtractionFromCobaltExport(zipFileName: String,
                                      outputPrefix: String,
                                      sentenceElement: String = "s",
                                      enhanceTags: Boolean = true,
                                      cleanBrackets: Boolean = false,
                                      info:Option[TrainingDataInfo] = None) {
  lazy val paths: Seq[Path] = zipUtils.find(zipFileName)


  case class Extractor() extends TrainingDataExtraction {

    override val split_test_train_on_document_level = !(outputPrefix.contains("evaluation_set")||info.exists(_.isSentenceLevel))// domme check....
    override lazy val output_prefix: String = outputPrefix
    override val sentence_element: String = sentenceElement
    override val enhance: Boolean = enhanceTags
    override val clean_brackets: Boolean = cleanBrackets

    override def pickPartition(fileId: Option[String], sentenceId: Option[String]): Partition =
      info.map(_.pickPartition(fileId,sentenceId)).getOrElse(super.pickPartition(fileId, sentenceId))
    // override lazy val output_folder = outputDir
  }

  def extract() = {
    val e = Extractor()
    println(e.output_folder)
    e.makeTrainingMaterialAndPartitionFromPaths(paths, e.output_prefix)
  }
}
