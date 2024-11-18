package corpusprocessing.gysseling
import posmapping._
import utils.PostProcessXML
import utils.PostProcessXML._

import scala.xml._
import utils.ProcessFolder

import java.io.File

object GysselingToTDNCore {

  val mappings: Map[String, Map[String,String]] = Map(
    "PD.type" -> Map("dem" -> "d-p", "rel" -> "d-p", "inter" -> "w-p"),
    "CONJ.type" -> Map("comp" -> "uncl", "expl" -> "uncl", "neg" -> "uncl", "rel" -> "sub"),
    "PD.position" -> Map("prenom|postnom|pred" -> "uncl"),
    "AA.position" -> Map("prenom|postnom|pred" -> "uncl"),
  )


  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/GysLiteraireSelectie/"
  val outDir = "/tmp/TestOut/"
  new File(outDir).mkdir()
  val file  = dir + "4000.tei.xml"
  val tagset = TagsetDiachroonNederlands
  val coreTagset = TagSet.fromXML("data/TDN/Corpora/Core/tagset_desc_temp.xml")


  def fixTag(t: Tag) = {
    val f1 = t.features.map(f => {
      val qname = s"${t.pos}.${f.name}"
      if (mappings.contains(qname) && mappings(qname).contains(f.value)) f.copy(value = mappings(qname)(f.value)) else f
    })
    val t1 = s"${t.pos}(${f1.map(f => s"${f.name}=${f.value}").mkString(",")})"
    tagset.integratedTag(t1)
  }

  def mapTag(tagStr: String) = {
    val tag = tagset.integratedTag(tagStr)
    val coreTag = fixTag(tagset.mapToCore(tag))
    val valid = coreTagset.isValid(coreTag)
    if (!valid) Console.err.println(s"$tagStr -> $coreTag ($valid)")

    coreTag
  }

  def mapTagInWord(w: Elem): Elem = {

    val lemmaRef = (w \ "@lemmaRef").text.replaceAll("[^ 0-9]", "").trim.split("\\s+").toList.sorted.mkString("-")//.map(_.toInt).sorted.map(_.toString).mkString("-")
    val tagjes = (w \ "@pos").text.split("\\+").toList.map(mapTag).map(_.toString)

    val w1 = replaceAttribute(w, "pos", tagjes.mkString("+"))
    val w2 = replaceAttribute(w1, "msd", (w \ "@pos").text)
    val w3 = if (lemmaRef.length == 0) w2 else w2.copy(child = w2.child ++ Seq(<join n={lemmaRef}/>))
    w3
  }
  def mapFile(d: Elem): Elem = {
    PostProcessXML.updateElement(d, _.label == "w", w => mapTagInWord(w))
  }

  def process(f1: String, f2: String) = {
    val d = XML.load(f1)
    val pid = (d \\ "interpGrp").find(x => (x \ "@type").text=="pid").headOption.map(x => (x \ "interp").text).getOrElse("pid_not_found")
    val d2 = mapFile(d)
    val d3 = PostProcessXML.replaceAttribute(d2, "xml:id", pid)
    XML.save(f2, d3)
  }
  def main(args: Array[String])  = {

    ProcessFolder.processFolder(new File(dir), new File(outDir), process)
  }
}
