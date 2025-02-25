package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.eindhoven.Eindhoven.getId

import java.io.File
import scala.xml._
object CheckLancelotTEI {
  val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/download_19_februari_2025/Gys/LancelotExport/"



  def getJoin_n(n: Node): String =  {
    if ((n \ "join").nonEmpty)
      (n \ "join" \  "@n").text;
    else if ((n \ "@corresp").nonEmpty) { // oude gysseling en cobalt codering......
      val join_n = (List(getId(n).get) ++  (n \ "@corresp").text.split("\\s+").map(_.replaceAll("#","")).toList).sorted.mkString("_")
      join_n
    } else "" }

  def fixPoSforPunct(w: Elem): Elem = {
    val txt:String = if ( (w \ "seg").nonEmpty) (w \ "seg").text.trim else (w.text.trim);
    if (txt.matches("(\\p{P})+")) {
      Console.err.println(s"Missed punctuation in $w")
      val w1 = corpusprocessing.parlamint.setAtt.setAttribute(w,"pos", "PC")
      w1.copy(label="pc")
    } else w
  }
  def fixJoinForW(w: Elem) = {
       if ( (w \ "join").nonEmpty || (w \ "@corresp").isEmpty )
         w else
         {
           val n = getJoin_n(w)
           // println(s"Fixing missing join, $n")
           w.copy(child = w.child ++ <join n={n}/>)
         }
    }

  def fixMissingJoins(d: Elem): Elem  = utils.PostProcessXML.updateElement(d, _.label=="w", fixJoinForW)
  def fixPunct(d: Elem)  = utils.PostProcessXML.updateElement(d, x => Set("w", "pc").contains(x.label), fixPoSforPunct)
  def patchDocument(d: Elem): Elem  = fixPunct(fixMissingJoins(d))

  def main(args: Array[String]): Unit = {
    new File(dir).listFiles().foreach(f => {
      val d = XML.loadFile(f)
      fixMissingJoins(d)
    })
  }
}
