package corpusprocessing.wolkencorpus

import corpusprocessing.brievenalsbuit.{LemPos, TagStuff}
import utils.{PostProcessXML, ProcessFolder}

import java.io.File
import scala.xml.{Elem, Null, UnprefixedAttribute, XML}

object postProcessBaBTagging {

  val inDir = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/Tagged/"
  val outDir = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/PostProcessed/"

  def fixW(e: Elem, ud: Boolean = true) = {
    val pos0 = if ((e \ "@pos").nonEmpty) (e \ "@pos").text else (e \ "@type").text

    val lem0 = (e \ "@lemma").text
    val word = e.text.trim
    //Console.err.println((pos0,lem0))
    val (pos, lem) = if (pos0.toLowerCase.trim.equals("y") && lem0.toLowerCase.trim.equals("x"))
      {
        Console.err.println(s"Bad $e")
        ("RES(type=uncl)", word)
      }
    else (pos0,lem0)

    val lp: LemPos = if (ud) TagStuff.parseLemPos(lem, pos).toUD.toStrings else TagStuff.parseLemPos(lem, pos).toTDN.toStrings

    val lemma: String = if (lp.lemma.nonEmpty) lp.lemma else word
    val newAtts =
      e.attributes.filter(a => !Set("type", "lemma").contains(a.key))
        .append(new UnprefixedAttribute("pos", lp.pos, Null))
        .append(new UnprefixedAttribute("lemma", lemma, Null))

    e.copy(attributes = newAtts)
  }

  def fixDocje(d: Elem, ud: Boolean = true) = {
    val d1 = PostProcessXML.updateElement(d, _.label == "w", x => fixW(x,ud))
    d1
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(inDir), new File(outDir), { case (i, o) =>
      if (i.endsWith(".xml")) {
        Console.err.println(s"Processing $i")
        val inDoc = XML.load(i)
        val outDoc = fixDocje(inDoc)
        XML.save(o, outDoc, "UTF-8")
      }
    })
    //val x = fixDocje(bartje)
  }
}
