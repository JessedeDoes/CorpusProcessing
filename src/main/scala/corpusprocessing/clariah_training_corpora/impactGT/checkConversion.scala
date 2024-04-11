package corpusprocessing.clariah_training_corpora.impactGT
import Settings._
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import java.io.File
import scala.xml._
object checkConversion {
  lazy val teiDocs = new java.io.File(teiDir).listFiles()
  def checkFile(f: File): Unit = {
    val doc = XML.loadFile(f)
    val id = (doc \\ "interp").filter(i => (i \ "@type").text == "pcgtsId").text.replaceAll("pc-","")
    val pageFile  = Settings.pageDir + "/"  + id  + ".page.xml"
    val pageDoc = XML.load(pageFile)
    val pageRegionIds = (pageDoc \\ "TextRegion").map(r => (r \ "@id").text).toSet
    val teiIds = doc.descendant.map(x => getId(x)).map(x => x.replaceAll(".*\\.","")).filter(_ != "no_id_found").toSet
    //println(teiIds)
    val missing = pageRegionIds diff teiIds
    if (missing.nonEmpty)
       println(s"${f.getName} (page $id): Missing $missing")

  }
  def main(args: Array[String])  = {
       teiDocs.foreach(f => checkFile(f))
  }
}
