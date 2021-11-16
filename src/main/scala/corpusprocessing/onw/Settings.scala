package corpusprocessing.onw

import corpusprocessing.metadata.addProcessedMetadataValuesONW
import posmapping.TagsetDiachroonNederlands

import scala.xml._

object Settings {

  val atHome = false
  val baseDir  = if (atHome) "/mnt/DiskStation/homes/jesse/work/ONW/"  else "/mnt/Projecten/Nederlab/corpusbouw/TEI/ONW/"
  val inputDir = "/home/jesse/workspace/Hilex/" + "data/ONW/" + "klaar_voor_Jesse_en_Mathieu"


  val tagsplitSourceDir = "/home/jesse/workspace/data-historische-corpora/ONW/TEI-postprocessed-v2/"
  val tagsplitTargetDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-tagsplit/"
  val processedMetadataDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata-v2"
  val gysselingProcessedMetadataDir = "/home/jesse/workspace/data-historische-corpora/gysseling/gysseling-processed-metadata"
  val gysselingEnhancedTEI = "/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-enhanced-tei"

  val eindhovenProcessedMetadataDir = "/mnt/Projecten/Nederlab/Tagging/TKV_Eindhoven/eindhovenProcessedMetadata/"
  val eindhovenEnhancedTEI = "/mnt/Projecten/Nederlab/Tagging/TKV_Eindhoven/metaPatch/"

  val clvnTagged = "/mnt/Projecten/Corpora/Historische_Corpora/CLVN/Tagged/"
  val clvnPostProcessed= "/mnt/Projecten/Corpora/Historische_Corpora/CLVN/PostProcessedMetadata/"
  val mnlDir = "/mnt/Projecten/corpora/Historische_Corpora/MNL-TEI/Nederlabversie/"

  val mnlTEI = "/mnt/Projecten/corpora/Historische_Corpora/MNL-TEI/Nederlabversie/CorpusMiddelnederlands/"
  val mnlDirkified = "/mnt/Projecten/corpora/Historische_Corpora/MNL-TEI/Nederlabversie/Metaculous/"
  val mnlTokenizedTEI = mnlDir + "TaggedV2"
  val mnlProcessedMetadataDir = mnlDir + "PostProcessedMetadata"

  val useCHNStyle = true

  val specPOS = if (useCHNStyle) "RES" else "SPEC"
  val specFeature = if (useCHNStyle) "type" else "spectype"
  val foreign = if (useCHNStyle) "foreign" else "vreemd"
  val specTag = if (useCHNStyle) "RES(type=foreign)" else "SPEC(vreemd)"

  val outputDir = if (atHome) "/data/ONW/postprocessed" else "/home/jesse/workspace/data-historische-corpora/ONW/TEI-postprocessed-v2"

  val extra = "/home/jesse/workspace/Hilex/" + "data/ONW/extraInfo.txt"

  val lwOnline = "https://digitalcollections.universiteitleiden.nl/view/item/881139/pages%3C#page/__/mode/1up"

  val crmDir:String =
    "/mnt/Projecten/corpora/Historische_Corpora/CRM/" // /mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM"

  val CRMtagmappedDir = crmDir + "/TEI-tagmapped"
  val CRMpostProcessedDir = crmDir + "/PostProcessedMetadata"

  def makePB(n: String): Elem =
  {
    val n1 = n.replaceAll("[^0-9]","").toInt
    val fol = if (n.contains("b")) s"${n1-9}r" else s"${n1 - 9-1}v"
    <pb unit="fol" nOrg={n} n={fol} ed={lwOnline} edRef={n1.toString}/>


  }

  def main(args: Array[String]): Unit = {
    onwCorpus.main(Array())
    splitONWTagsInFeatures.main(Array())
    addProcessedMetadataValuesONW.main(Array())
    // TagsetDiachroonNederlands.doONW
  }
}
