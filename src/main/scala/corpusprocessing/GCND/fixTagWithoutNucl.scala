package corpusprocessing.GCND
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.AlpinoSentence
import utils.PostProcessXML
import utils.PostProcessXML.replaceAttribute

import scala.xml._
import utils.PostProcessXML

object fixTagWithoutNucl {
  def fixTag(node: Elem): Elem  = {
    Console.err.println("Golly, removing tag!"  + node.copy(child=Seq()))
    node.copy(child = node.child.map(
      c => c match {
        case e: Elem if rel(e) == "tag" => replaceAttribute(e,"rel", "--")
        case _ => c
      }
    ))
  }

  def rel(n: Node) = (n \ "@rel").text

  def fixTagWithoutNucl(x: Elem)  = {
    PostProcessXML.updateElement3(x,
      n => n.label == "node" & n.child.exists(x => rel(x) == "tag") && !(n.child.exists(rel(_) == "nucl")), fixTag)
  }
}
