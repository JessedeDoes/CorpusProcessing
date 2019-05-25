package posmapping
import scala.xml._
import java.io.File
import utils.PostProcessXML._

object splitONWTagsInFeatures {
  //val tagset = CHNStyleTags

  val sourceDir = "/home/jesse/workspace/data-historische-corpora/ONW/TEI-postprocessed/"
  val targetDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-tagsplit/"


  def Ѧ(n:String, v: String) = new UnprefixedAttribute(n,v,Null)

  def replaceAtt(m: MetaData, name: String, value: String): MetaData = m.filter(a => a.key != name).append(Ѧ(name, value))


  def splitFS(fs: Elem, lemma: String): Seq[Elem] =
  {
    val fs1 = fs.copy(
      attributes = replaceAtt(fs.attributes, "type", "chnpos"),
      child = fs.child.flatMap({
        case e: Elem if e.label == "f" && (e \ "@name").text.startsWith("pos.")  => {
          val newName = (e \ "@name").text.replaceAll("pos.","")
          e.copy(attributes = replaceAtt(e.attributes, "name", newName)) }
        case _ => Seq()
      }) ++ Seq(<f name="lemma">{lemma}</f>)
    )
    Seq(fs,fs1)
  }

  def updateTag(w: Elem):Node =
  {
    val lemma = (w \ "@lemma").text
    val lemmata = lemma.split("\\+").toSeq
    val fsjes = (w \ "fs").zip(lemmata).flatMap(
      {case (fs, l) =>  splitFS(fs.asInstanceOf[Elem], l)}
    )
    val newChild = w.child.filter(_.label != "fs") ++ fsjes
    // Console.err.println(newChild)
    w.copy(child = newChild)
  }

  val noNo: String = new Character(133).toString

  def cleanText(n: Node): Node =
  {
    n match {
      case e: Elem => {
        e.copy(child = e.child.map(cleanText))
      }
      case t: Text => Text(t.text.replaceAll(noNo, " "))
      case _ => n
    }
  }

  def updateTags(d: Elem):Node =
  {
    updateElement2(d, _.label=="w", updateTag).head
  }

  def doFile(f1: String, f2: String) =
  {
    val d = XML.load(f1)
    val d1 = updateTags(d)
    XML.save(f2,cleanText(d1),"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(sourceDir), new File(targetDir), doFile)
  }
}
