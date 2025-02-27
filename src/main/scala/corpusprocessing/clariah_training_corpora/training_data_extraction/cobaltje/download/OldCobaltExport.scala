package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.download

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.{LancelotDatabase, OldCobaltSettings}

object OldCobaltExport {
  def main(args: Array[String]) = {
    val stats = LancelotDatabase(OldCobaltSettings)
    LancelotExport.checkAllCorpora(stats, OldCobaltSettings)
  }
}
