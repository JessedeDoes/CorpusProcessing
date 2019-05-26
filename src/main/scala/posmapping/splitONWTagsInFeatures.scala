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

  def padYear(s: String) = {
    val s1 = s.trim
    if (s1.matches("^[0-9]{1,4}$")) {
      val s2 = (0 until (4 - s.length)).map(x => "0").mkString("") + s1
      Console.err.println(s"$s1 --> $s2")
      s2
    }
    else s1
  }

  def padYears(grp: Elem) = {
     val r = grp.copy(child = grp.child.map(cleanText(padYear)))
     if ((r \ "@type").text.toLowerCase().contains("year"))
       Console.err.println(r)
    r
  }

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

  def cleanText(f: String => String)(n: Node): Node =
  {
    n match {
      case e: Elem => {
        e.copy(child = e.child.map(cleanText(f)))
      }
      case t: Text => Text(f(t.text))
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
    val d2 = updateElement(d1.asInstanceOf[Elem], x => x.label=="interpGrp" && (x \ "@type").text.toLowerCase.contains("year"), padYears )
    XML.save(f2,cleanText(s => s.replaceAll(noNo, " "))(d2.asInstanceOf[Elem]),"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(sourceDir), new File(targetDir), doFile)
  }
}
