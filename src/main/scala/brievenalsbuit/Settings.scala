package brievenalsbuit

import java.io.File

object Settings {
  val atHome = false
  val useCGNStyle = false

  val babDir = if (atHome) "/mnt/DiskStation/homes/jesse/work/BaB/" else
    "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/LexiconToolCorpora/Zeebrieven/TKV/"

  val input = new File(babDir + "2.0")
  val output = new File(babDir + (if (useCGNStyle) "2.5CGN/" else "2.5CHN/"))
  val discardSubElemsOfWord = true
  val multivalSepSplit = "[+]"
  val multivalSepPrint = "+"
  val alternativeSep = "[|]"
  val alternativePrint = "|"
}
