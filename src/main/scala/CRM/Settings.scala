package CRM

object Settings
{
  val atHome = false
  val lumpIt = false // corpus in een XML?
  val keepRegOrig = false

  val doAll = true
  val maxDocs = if (doAll) Int.MaxValue else 100
  val dir:String = if (atHome) "/home/jesse/data/CRM/" else
    "/mnt/Projecten/corpora/Historische_Corpora/CRM/" // /mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM"
  val outputDir = dir + "/TEI"
  val postProcessedDir = dir + "/TEI-tagmapped"
  val CRM:String = dir + "Bron/CRM14Alfabetisch.txt"
  val index:String = dir + "Bron/index"

}
