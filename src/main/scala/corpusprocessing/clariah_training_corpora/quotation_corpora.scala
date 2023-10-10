package corpusprocessing.clariah_training_corpora

object quotation_corpora {

}


case class QuotationCorpus(sourceFolder: String, name: String) extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = name
  override val max_files: Int = Integer.MAX_VALUE
  override val output_folder: String = sourceFolder + "/" + "test_train"
  override val default_folder = sourceFolder
}

object QuotationCorpora {
  object q18 extends QuotationCorpus()
}