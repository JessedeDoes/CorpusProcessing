package posmapping

import java.io.PrintWriter

object IntegratedTagset {
   val tagset = TagSet.fromXML("data/Molex/combined_tagset_desc.xml")

   case class IntegratedTag(tag: String) extends CHNStyleTag(tag, tagset)

   val GysTags = scala.io.Source.fromFile("data/CG/overzichtje_met_morfcode.txt").getLines.map(l => l.split("\\s+"))// .map(l => l(2))
   def main(args: Array[String]): Unit = {
      val patchedTagset = new PrintWriter("/tmp/gystags.out")
      GysTags.foreach(ts => {
         val (m,t1,t) = (ts(0),ts(1), ts(2))
         val restje = ts.drop(3).mkString(" ")
         val tPatched =HistoricalTagsetPatching.patchPoSMistakes(m,t1,"")
         val tag = IntegratedTag(tPatched)
         val v = tagset.isValid(tag)
         patchedTagset.println(s"${ts(0)}\t$tPatched\t$restje)")
      })
      patchedTagset.close()
   }
}

