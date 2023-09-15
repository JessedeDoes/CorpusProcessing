package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import scala.xml.Node

case class AlpinoToken(n: Node, id: Option[String] = None) {
  val atts: Map[String, String] = n.attributes.map(n => n.key -> n.value.text).toMap
  lazy val lemma: String = atts.getOrElse("lemma", "_")
  lazy val word: String = atts.getOrElse("word", "_")
  lazy val postag: String = atts.getOrElse("postag", "_")
  val begin: Int = atts("begin").toInt
}
