package corpusprocessing.gentse_spelen
import utils.PostProcessXML.{updateElement, updateElement4}

import scala.xml._

object postprocessing {
   val gsRetagged="/mnt/Projecten/Corpora/Historische_Corpora/GentseSpelen/Retagged/gent.15Mei2013.export.xml"
  val gsPostprocessed ="/mnt/Projecten/Corpora/Historische_Corpora/GentseSpelen/Retagged/gent.15Mei2013.export.postprocessed.2.xml"
  val overbodigeAttributen = Set("xtype", "time", "resp",  "ctag", "subtype", "original", "old_pos", "old_lemma", "resp", "type", "mform", "nform", "lemma") // misAlignent, changed


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
    XML.save(gsPostprocessed, d2, enc = "utf-8")
  }
}
