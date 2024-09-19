package corpusprocessing.GCND

import database.Configuration

object Settings {
  val foliaDir = "/home/jesse/WorkSpace/GCND/Folia/"
  val databaseConfig = new Configuration(name="gcnd.nogmaals", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd")

  val extractSentencesFromFoliaStylesheet = "data/GCND/XSL/extract_sentences.xsl"

  val targetDirectoryForAlpinoSentencesForGretel = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/Alpino/"
}
