package corpusprocessing.clariah_training_corpora.enhanceBaB

import scala.xml._
import utils.PostProcessXML._
import utils.ProcessFolder._


// Dit hoeft dus helemala niet. Niet doen!!
object tweakNELemmata {
  val babPath = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/SelectionForEnhancementTagged/"
  val outputPath = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/SelectionForEnhancementTweaked/"
  implicit def f(fn: String)  = new java.io.File(fn)

  def dow(w: Elem)  = {
    val lem = (w \ "@lemma").text
    if ((w \ "@pos").text.contains("NOU-P") && !lem.matches("^[A-Z].*")) println(lem + " : " + w \ "@pos")
    w
  }
  def tweakLemmata(in: String, out: String)  = {
    val d = XML.load(in)
    val d1 = updateElement(d, _.label == "w", dow)
    // println(out)
    // XML.save(out, d1)
  }

  def main(args: Array[String])  = {
    processFolder(babPath, outputPath, tweakLemmata)
  }
}
