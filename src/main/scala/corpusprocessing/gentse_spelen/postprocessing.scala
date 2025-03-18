package corpusprocessing.gentse_spelen
import utils.PostProcessXML.{updateElement, updateElement4, updateElement5}

import scala.xml._

object postprocessing {
   val gsRetagged="/mnt/Projecten/Corpora/Historische_Corpora/GentseSpelen/Retagged/gent.15Mei2013.export.xml"
  val gsPostprocessed ="/mnt/Projecten/Corpora/Historische_Corpora/GentseSpelen/Retagged/gent.15Mei2013.export.postprocessed.2.xml"
  val overbodigeAttributen = Set("xtype", "time", "resp",  "ctag", "subtype", "original", "old_pos", "old_lemma", "resp", "type", "mform", "nform", "lemma") // misAlignent, changed

  def tokenize(w: Elem) = {
    val word = (w \ "seg").text
    val pos = (w \ "@pos").text
    if (pos.matches(".*abbr.*") || !word.endsWith(".")) {
      if (word.endsWith(".")) Console.err.println(s"Keeping dot in $word, $pos")
      w
    } else {
      // Console.err.println(s"Splitting dot in $word, $pos")
      val w1 = word.replaceAll("\\.$", "")
      val pc = "."
      Seq(
        updateElement(w, _.label == "seg", s => <seg>{w1}</seg>),
        <pc>.</pc>
      )
    }
  }

  def main(args:Array[String]) = {
    val d = XML.load(gsRetagged)

    val d1 = updateElement(d, _.label=="w", w => {
      val oldlemma = (w \ "@old_lemma").text
      val msd =(w \ "@ctag").text
      w.copy(
        attributes = w.attributes.filter(a => !overbodigeAttributen.contains(a.key))
          //.append(new UnprefixedAttribute("msd", cgnPossen2.mkString(multivalSepPrint), Null) )

          .append(new UnprefixedAttribute("msd", msd, Null) )
          .append(new UnprefixedAttribute("lemma", oldlemma, Null)))
    })

    def noWIn(e: Elem)  = (e \\ "w").isEmpty
    def noSeg(e: Elem)  = e.copy(child = e.child.filter(_.label != "seg"))
    val d2 = updateElement(d1, _.label=="text",
      t => updateElement(t, x => !Set("w", "pc", "seg").contains(x.label) && noWIn(x), x => x.copy(child=Seq()))
    )
    val d3 = updateElement5(d2, _.label == "w", tokenize).asInstanceOf[Elem]
    XML.save(gsPostprocessed, d3, enc = "utf-8")
  }
}
