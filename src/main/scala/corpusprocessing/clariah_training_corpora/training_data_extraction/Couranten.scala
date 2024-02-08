package corpusprocessing.clariah_training_corpora.training_data_extraction

case class CourantenCorpus(sourceFolder: String, name: String) extends extract_training_data_trait {
  override val sentence_element="s"
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = name
  override val max_files: Int = Integer.MAX_VALUE
  override val training_subsets: Int = 1
  override lazy val output_folder: String = sourceFolder + "/" + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  // new java.io.File(output_folder).mkdir()
  override lazy val default_folder = sourceFolder
}

object Couranten {
  def main(args: Array[String]) = {
    CourantenCorpus ("/home/jesse/Downloads/CobaltServeExport/", "couranten").main(args)
  }
}