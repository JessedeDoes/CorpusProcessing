package corpusprocessing.GCND

import folia.FoliaToRudimentaryTEI
import folia.FoliaToRudimentaryTEITrait
import utils.ProcessFolder
import scala.xml._
import java.io.File
import utils.PostProcessXML
// conversie naar kale TEI, om tagger etc te kunnen draaien en weer terug te foefelen in de folia

case class back2tei() extends FoliaToRudimentaryTEITrait() {

  override val utterance_element: String = "s"
  override def convertWord(w: Node): Seq[Node] = {

    val id = w \ s"${xml}id"
    val norm  = (w \ "t").filter(x => (x \ "@class").text.contains("heavy")).text
    val orig  = (w \ "t").filter(x => (x \ "@class").text.contains("light")).text
    <w orig={orig} xml:id={id}>{norm}</w>
  }
}
object ConvertToBasicTEI {
  val test = "/home/jesse/workspace/XmlToRdf/data/GCND/Folia/gcnd.983.folia.xml"

  def convert(d: Elem) = {
    val d0 = PostProcessXML.updateElement5(d, _.label=="s", e => Seq()).asInstanceOf[Elem]
    val d1 = back2tei().convert(d)
    // println(d1)
    d1
  }

  def doit(in: String, out:String) = {
    val d1 = XML.load(in)
    val d2 = convert(d1)
    XML.save(out.replaceAll("folia.xml", "tei.xml"), d2)
  }
  def main(args: Array[String])  = {
    ProcessFolder.processFolder(new File("/home/jesse/workspace/XmlToRdf/data/GCND/Folia"), new File("/home/jesse/workspace/XmlToRdf/data/GCND/TEI"), doit)
    // convert(XML.load("/home/jesse/workspace/XmlToRdf/data/GCND/Folia/gcnd.983.folia.xml"))
  }
}


