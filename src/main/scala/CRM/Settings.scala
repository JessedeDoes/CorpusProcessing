package CRM

object Settings
{
  val atHome = true
  val lumpIt = false // corpus in een XML?
  val keepRegOrig = false

  val doAll = false
  val maxDocs = if (doAll) Int.MaxValue else 100
  val dir:String = if (atHome) "/home/jesse/data/CRM/" else "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/"
  val outputDir = dir + "/TEI"
  val CRM:String = dir + "CRM14Alfabetisch.txt"
  val index:String = dir + "index"

}
