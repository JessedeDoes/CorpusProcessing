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

/*
mood=imperative not ok for VRB
number=other not ok for AA
number=pl not ok for AA
number=pl not ok for PD
number=sg not ok for AA
position=adverbial not ok for AA
pos=NOU not ok for NOU
subtype=other not ok for ADV
subtype=other not ok for CONJ
subtype=resumptive not ok for ADV
tense=present not ok for VRB
type=art not ok for PD
type=coord not ok for CONJ
type=general not ok for ADP
type=general not ok for ADV
type=interjection not ok for INT
type=refl/recp not ok for PD

 */