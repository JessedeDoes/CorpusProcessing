package corpusprocessing.onw

import java.io.File
import java.util.Comparator

import scala.xml._
import utils.PostProcessXML._
import utils.Tokenizer

object foezelWatMetBestand {

  def doWord(w: Elem) =
  {
    val w1 = w.child.filter(_.label != "fs").map(_.text).mkString.trim
    val newChild = (w \\ "fs").map(
      n => {
        val fs = n.asInstanceOf[Elem]
        val lemma = (n \ "f").filter(x => (x \ "@name").text == "lemma").text
        val pos = (n \ "@pos")
        <m type="cliticPart">{n}</m>
      }
    )
    w.copy(attributes =w.attributes.append(new UnprefixedAttribute("word", w1, Null)))
  }

  def doIets(f: String): Unit =
  {
    val d = XML.load(f)

    val d1 = updateElement2(d, _.label == "w", doWord)
  }
}
