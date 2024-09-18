package corpusprocessing.GCND

import database.Configuration

object Settings {
  val foliaDir = "/home/jesse/WorkSpace/GCND/Folia/"
  val config = new Configuration(name="gcnd.nogmaals", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd")
}
