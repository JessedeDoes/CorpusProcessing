package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import scala.xml._

case class AlpinoNode(s: AlpinoSentence, n: Node) {
  val id = (n \ "@id").text
  val word = (n \ "@word").text
  val isLeaf = word.nonEmpty // (n \\ "node").isEmpty
  val rel = (n \ "@rel").text
  val begin = (n \ "@begin").text
  val beginBaseOne = ((n \ "@begin").text.toInt + 1).toString
  lazy val parent: Option[AlpinoNode] = s.parentMap.get(id).flatMap(pid => s.nodeMap.get(pid))

  override def toString: String = n.toString()
}

case class AlpinoSentence(e: Elem) {
  val nodeMap: Map[String, AlpinoNode] = (e \\ "node").map(n => AlpinoNode(this, n)).map(n => n.id -> n).toMap
  val wordMap: Map[String, AlpinoNode] = (e \\ "node").map(n => AlpinoNode(this, n)).filter(_.isLeaf).map(n => n.beginBaseOne -> n).toMap
  // println(wordMap.keySet)
  val parentMap = nodeMap.flatMap(n => {
    val childIds = n._2.n.child.map(n => (n \ "@id").text).filter(_.nonEmpty)
    childIds.map(i => i -> n._2.id)
  }).toMap
}
