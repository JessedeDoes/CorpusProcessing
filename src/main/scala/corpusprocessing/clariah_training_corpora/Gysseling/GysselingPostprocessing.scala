package corpusprocessing.clariah_training_corpora.Gysseling

import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import utils.{PostProcessXML, ProcessFolder}

import java.io.File
import scala.xml.{Elem, NodeSeq, Text, XML}

object GysselingPostprocessing {

  def adaptLemma(w: Elem) = {
    val lemmata = (w \ "@lemma").text.split("\\+").toList
    val posje = (w \ "@pos").text
    val posjes = (w \ "@pos").text.split("\\+").toList
    val wordform = (w \ "seg").text
    val newlem = if (lemmata.size == posjes.size) {
      val newLems = lemmata.zip(posjes).map({ case (l, p) =>
        if (p.startsWith("NOU-P")) {
          val l0 = l.take(1)
          val l1 = l.drop(1)
          l0 + l1.toLowerCase
        } else {
          val l0 = l.toLowerCase()
          if (l0.isEmpty || l0.equals("zzz")) {
            if (!posje.startsWith("RES")) Console.err.println(s"$wordform $posje: lemma wordt $wordform")
            wordform
          } else l0
        }
      })
      newLems.mkString("+")
    } else (w \ "@lemma").text.toLowerCase()
    val posje_pos = posje.replaceAll("degree=uncl", "degree=pos")
    setAtt.setAttribute(setAtt.setAttribute(w, "lemma", newlem), "pos", posje_pos)
  }


  def splitContiguousParts(w: Elem): NodeSeq = {
    val word = (w \\ "seg").text.trim
    val id: String = getId(w)
    val wordParts = word.split("\\s+").toSeq
    if (wordParts.size > 1) {
      wordParts.indices.map(wpi => {
        val wp = wordParts(wpi)
        val adapted1 = PostProcessXML.updateElement(w, _.label == "seg", s => s.copy(child = Text(wp)))
        val adapted2 = setAtt.setAttribute(adapted1, "xml", "id", id + "." + wpi)
        val otherIndexes = wordParts.indices.filter(_ != wpi)
        val newCorresp = ((w \ "@corresp").text + " " + otherIndexes.map(i => s"#${id + "." + i}").mkString(" ")).trim
        val adapted3 = setAtt.setAttribute(adapted2, "corresp", newCorresp)
        val adapted4 = setAtt.setAttribute(adapted3, "feest", "hoera")
        println(adapted4)
        adapted4
      })
    } else Seq(w)
  }


  def addJoins(w: Elem): Elem = {
    val id: String = getId(w)
    val word = (w \\ "seg").text.trim

    val corresps: List[String] = (w \ "@corresp").text.split("\\s+").filter(_.nonEmpty).map(_.replaceAll("#", "").trim).toList
    if (corresps.isEmpty)
      w;
    else {
      val groupId = (List(id) ++ corresps).sorted.mkString("_")
      w.copy(child = w.child ++ Seq(<join n={groupId}/>))
    }
  }

  def adaptLemmata(d: Elem) = {

    val d1 = PostProcessXML.updateElement5(d,
      _.label == "w",
      w => splitContiguousParts(adaptLemma(w))).asInstanceOf[Elem]

    PostProcessXML.updateElement5(d1, _.label == "w",
      w => addJoins(w)).asInstanceOf[Elem]
  }

  def doit(in: String, out: String) = {
    val d = XML.load(in)
    val d1 = adaptLemmata(d)
    XML.save(out, d1)
  }

  val part = "Ambtelijk" // "Literaire"
  lazy val in = new File(s"/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/Gys${part}SelectieTDN")
  lazy val out = new File(s"/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/Gys${part}SelectieTDNPostProcessed")

  def main(args: Array[String]) = {
    out.mkdir()
    ProcessFolder.processFolder(in, out, doit)
  }
}
