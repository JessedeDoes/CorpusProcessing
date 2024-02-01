package corpusprocessing.clariah_training_corpora.training_data_extraction


case class PapCorpus(sourceFolder: String="/mnt/Projecten/Papiaments/Corpusdata/madlad_400/", name: String = "pap") extends extract_training_data_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = name
  override val max_files: Int = Integer.MAX_VALUE
  //override val training_subsets: Int = 10
  override lazy val output_folder: String = sourceFolder + "/" + "lm_test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  // new java.io.File(output_folder).mkdir()
  override lazy val default_folder =  "/mnt/Projecten/Papiaments/Corpusdata/madlad_400/tokenized/" // sourceFolder + "/" + "CobaltServeExport"
}

object lm_trainin_data_for_pap extends PapCorpus() {

}
