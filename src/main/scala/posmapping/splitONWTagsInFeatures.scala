package posmapping
import scala.xml._
import java.io.File
import utils.PostProcessXML._

object splitONWTagsInFeatures {
  //val tagset = CHNStyleTags

  val sourceDir = "/home/jesse/workspace/data-historische-corpora/ONW/TEI-postprocessed/"
  val targetDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-tagsplit/"

  val files = new File(sourceDir).listFiles()

  def Ѧ(n:String, v: String) = new UnprefixedAttribute(n,v,Null)

  def replaceAtt(m: MetaData, name: String, value: String) = m.filter(a => a.key != name).append(Ѧ(name, value))


  def splitFS(fs: Elem) =
  {
    val fs1 = fs.copy(
      attributes = replaceAtt(fs.attributes, "type", "chnpos"),
      child = fs.child.flatMap({
        case e: Elem if (e.label == "f" && (e \ "@name").text.startsWith("pos.") ) => {
          val newName = (e \ "@name").text.replaceAll("pos.","")
          e.copy(attributes = replaceAtt(e.attributes, "name", newName)) }
        case _ => Seq()
      })
    )
    Seq(fs,fs1)
  }

  def updateTag(d: Elem):Node =
  {
    updateElement2(d, _.label=="fs", splitFS).head
  }

  def doFile(f1: String, f2: String) =
  {
    val d = XML.load(f1)
    val d1 = updateTag(d)
    XML.save(f2,d1,"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(sourceDir), new File(targetDir), doFile)
  }
}
