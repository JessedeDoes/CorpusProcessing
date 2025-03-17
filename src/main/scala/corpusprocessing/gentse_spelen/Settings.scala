package corpusprocessing.gentse_spelen

import java.io.File

object Settings {
  val atHome = false
  val useCGNStyle = false



  val dirOpCorpora = "/mnt/Projecten/Corpora/Historische_Corpora/GentseSpelen/"

  val input = new File(dirOpCorpora + "CobaltExport")
  val output = new File(dirOpCorpora + (if (useCGNStyle) "2.5CGN/" else "TDN/"))
  val discardSubElemsOfWord = true
  val multivalSepSplit = "[_+]"
  val multivalSepPrint = "+"
  val alternativeSep = "[|]"
  val alternativePrint = "|"
  val addPartInfoInTag = false
}
