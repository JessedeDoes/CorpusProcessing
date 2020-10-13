package corpusprocessing.brievenalsbuit

import java.io.File

object Settings {
  val atHome = false
  val useCGNStyle = false

  val babDir = if (atHome) "/mnt/DiskStation/homes/jesse/work/BaB/" else
    "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/LexiconToolCorpora/Zeebrieven/TKV/"

  val babDirOpCorpora = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuit/"

  val input = new File(babDirOpCorpora + "XML")
  val output = new File(babDirOpCorpora + (if (useCGNStyle) "2.5CGN/" else "2.7CHN/"))
  val discardSubElemsOfWord = false
  val multivalSepSplit = "[_+]"
  val multivalSepPrint = "+"
  val alternativeSep = "[|]"
  val alternativePrint = "|"
}
