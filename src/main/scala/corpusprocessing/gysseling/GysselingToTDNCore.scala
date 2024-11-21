package corpusprocessing.gysseling
import corpusprocessing.clariah_training_corpora.Gysseling.GysselingPostprocessing
import posmapping._
import utils.PostProcessXML
import utils.PostProcessXML._

import scala.xml._
import utils.ProcessFolder

import java.io.{File, PrintWriter}

object GysselingToTDNCore {

  val pairs : collection.mutable.HashMap[(String,String),Int] = new collection.mutable.HashMap[(String,String),Int]()

  def countPair(a: String, b: String)  = {
    try {
      if (pairs.contains((a, b))) pairs.put((a, b), pairs(a, b) + 1) else pairs.put((a, b), 1)
    }
  }

  val mappings: Map[String, Map[String,String]] = Map(
    "PD.type" -> Map("dem" -> "d-p", "rel" -> "d-p", "inter" -> "w-p"),
    "CONJ.type" -> Map("comp" -> "sub", "expl" -> "sub", "neg" -> "sub", "rel" -> "sub", "qual" -> "sub"),
    "ADP.type" -> Map("uncl" -> "pre"),
    "PD.position" -> Map("prenom|postnom|pred" -> "prenom"),
    "AA.position" -> Map("prenom|postnom|pred" -> "prenom"),
    "NUM.representation" -> Map("uncl" -> "let"),
  )


  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/GysAmbtelijkSelectie/Origineel/" // GysLiteraireSelectie/"
  val outDir = "/tmp/TestOut/"

  new File(outDir).mkdir()

  val file  = dir + "4000.tei.xml"
  val tagset = TagsetDiachroonNederlands
  val coreTagset = TagSet.fromXML("data/TDN/Corpora/Core/tagset_desc_temp.xml")


  def fixTag(t: Tag, tagOrg: String) = {
    val f1 = t.features.map(f => {
      val qname = s"${t.pos}.${f.name}"
      if (mappings.contains(qname) && mappings(qname).contains(f.value)) f.copy(value = mappings(qname)(f.value)) else f
    })
    val t1 = s"${t.pos}(${f1.map(f => s"${f.name}=${f.value}").mkString(",")})"
    val t2 = if (isRelPron(tagOrg) || isReflPron(tagOrg))  {
      val x = t1.replaceAll("position=(uncl|prenom)", "position=free")
      //println(s"booh $x");
      x
    } else t1
    tagset.integratedTag(t2)
  }


  def mapTag(tagOrg: String): CHNStyleTag = {
    val tag = tagset.integratedTag(tagOrg)
    val coreTag = fixTag(tagset.mapToCore(tag), tagOrg)
    // println(coreTag + "< "  + tagOrg)
    val valid = coreTagset.isValid(coreTag)
    if (!valid) Console.err.println(s"$tagOrg -> $coreTag ($valid)")

    coreTag
  }

  def isRelPron(t: String)  = t.matches("PD.*rel.*")
  def isReflPron(t: String)  = t.matches("PD.*ref.*")

  def mapTagInWord(w: Elem): Elem = {

    val lemmaRef = (w \ "@lemmaRef").text.replaceAll("[^ 0-9]", "").trim.split("\\s+").toList.sorted.mkString("-")//.map(_.toInt).sorted.map(_.toString).mkString("-")
    val tagjes = (w \ "@pos").text.split("\\+").toList//.map(mapTag).map(_.toString)
    val tagjesMapped = tagjes.map(mapTag).map(_.toString)
    val lemmata = (w \ "@lemma").text.split("\\+").toList

    val tagjesFixed: Seq[String] = tagjesMapped.zip(lemmata).map({
      case (t,l) if l.startsWith("W") && t.startsWith("PD") => t.replaceAll("d-p", "w-p")
      case (t,l) => t
    }).map(_.replaceAll("position=uncl", "position=prenom").replaceAll("degree=uncl", "degree=pos").replaceAll("[(][)]", ""))

    tagjes.zip(tagjesFixed).foreach({case (a,b) => countPair(a.replaceAll(",?inflection=[^,()]*",""),b)})

    val w1 = replaceAttribute(w, "pos", tagjesFixed.mkString("+"))
    val w2 = replaceAttribute(w1, "msd", (w \ "@pos").text)
    val w2b = w2.copy(child=w2.child.filter(x => !Set("fs","xr", "nolink").contains(x.label)))
    val w3 = if (lemmaRef.length == 0) w2b else w2b.copy(child = w2b.child ++ Seq(<join n={lemmaRef}/>))
    w2b
  }

  def mapFile(d: Elem): Elem = {
    PostProcessXML.updateElement(d, _.label == "w", w => mapTagInWord(w))
  }

  def process(f1: String, f2: String) = {
    val d = XML.load(f1)
    val pid = (d \\ "interpGrp").find(x => (x \ "@type").text=="pid").headOption.map(x => (x \ "interp").text).getOrElse("pid_not_found")
    val d2 = mapFile(d)
    val d3 = PostProcessXML.replaceAttribute(d2, "xml:id", pid)
    val d4 = GysselingPostprocessing.adaptLemmata(d3)
    XML.save(f2, d4)
  }

  def main(args: Array[String])  = {

    ProcessFolder.processFolder(new File(dir), new File(outDir), process)
    val p = new PrintWriter("/tmp/mapping.txt")
    pairs.keySet.toList.sorted.foreach(k => p.println(s"${k._1}\t${k._2}\t${pairs(k)}"))
    p.close()
  }
}
