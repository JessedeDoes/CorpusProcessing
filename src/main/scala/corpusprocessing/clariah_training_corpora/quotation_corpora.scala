package corpusprocessing.clariah_training_corpora

object quotation_corpora {

}


case class QuotationCorpus(sourceFolder: String, name: String) extends tei_to_huggingface_trait {
  override val split_test_train_on_document_level: Boolean = true
  override val output_prefix: String = name
  override val max_files: Int = Integer.MAX_VALUE
  override val training_subsets: Int = 10
  override val output_folder: String = sourceFolder + "/" + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  new java.io.File(output_folder).mkdir()
  override val default_folder = sourceFolder + "/" + "CobaltServeExport"
}

object QuotationCorpora {
  val baseDir  = "/mnt/Projecten/Corpora/TrainingDataForTools/galahad-corpus-data/public-corpora/clariah-evaluation-corpora/"
}

import QuotationCorpora._

object q15 extends QuotationCorpus(baseDir + "gtbcit_15", "gtbcit_15")
object q16 extends QuotationCorpus(baseDir + "gtbcit_16", "gtbcit_16")
object q17 extends QuotationCorpus(baseDir + "gtbcit_17", "gtbcit_17")
object q18 extends QuotationCorpus(baseDir + "gtbcit_18", "gtbcit_18")
object q19 extends QuotationCorpus(baseDir + "gtbcit_19", "gtbcit_19")