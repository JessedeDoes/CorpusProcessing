package corpusprocessing.mimore
import scala.xml._
import utils.PostProcessXML._

object Sandgroups {

  val inDir = new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/Mimore/Corpus/Sand_interviews")
  val outdir = new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/Mimore/Sand_grouped")

   def groupItems(interview: Elem) = {
     val items = (interview \ "s").toList //.reverse
     val grouped = groupWithFirst(items, s => s.text.matches("(?s).*\\[v=[0-9].*")) // s => (s \ "word").nonEmpty)
     val unpacked = grouped.map(g => {
       <item>{g}</item>
     })
     interview.copy(child = unpacked)
   }

  def doit(in: String, out: String) = {
    val inDoc = XML.load(in)
    val splitDoc = updateElement(inDoc, _.label=="interview", groupItems)
    XML.save(out, splitDoc, "UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(inDir, outdir, doit)
  }
}
