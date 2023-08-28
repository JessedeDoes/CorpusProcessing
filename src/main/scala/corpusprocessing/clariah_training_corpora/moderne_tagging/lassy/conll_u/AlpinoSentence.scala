package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import scala.xml._

case class AlpinoToken(n: Node)  {
  val atts = n.attributes.map(n => n.key -> n.value.text).toMap
  lazy val lemma = atts.getOrElse("lemma","_")
  lazy val word = atts.getOrElse("word","_")
  lazy val postag = atts.getOrElse("postag","_")
  val begin = atts("begin").toInt
}
case class AlpinoNode(s: AlpinoSentence, n: Node) {
  val id = (n \ "@id").text
  val word = (n \ "@word").text
  val isLeaf = word.nonEmpty // (n \\ "node").isEmpty
  val rel = (n \ "@rel").text
  val begin = (n \ "@begin").text
  val wordNumber = begin.toInt
  val beginBaseOne = ((n \ "@begin").text.toInt + 1).toString
  lazy val parent: Option[AlpinoNode] = s.parentMap.get(id).flatMap(pid => s.nodeMap.get(pid))

  override def toString: String = n.toString()
}

case class AlpinoSentence(alpino: Elem) {
  val nodeMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).map(n => n.id -> n).toMap
  val wordMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).filter(_.isLeaf).map(n => n.beginBaseOne -> n).toMap
  lazy val words = wordMap.values.toList.sortBy(_.wordNumber)
  // println(wordMap.keySet)
  val parentMap = nodeMap.flatMap(n => {
    val childIds = n._2.n.child.map(n => (n \ "@id").text).filter(_.nonEmpty)
    childIds.map(i => i -> n._2.id)
  }).toMap

  lazy val input_transcript = (alpino \\ "comment").text.replaceAll("^.*?\\|", "")
  lazy val id = ((alpino \\ "sentence").head \ "@sentid").text
  lazy val alpinoTokens = (alpino \\ "node").filter(x => (x \ "@word").nonEmpty).map(AlpinoToken).sortBy(_.begin)
  lazy val xml = <s xml:id={id}>
    {"\n"}{alpinoTokens.sortBy(_.begin).flatMap(t => Text("\t") :+ <w pos={t.postag} lemma={t.lemma}>
      {t.word}
    </w> +: Text("\n"))}
  </s>
}
