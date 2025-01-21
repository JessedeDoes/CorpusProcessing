package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u
import scala.util.Failure
import scala.util.Success

import scala.collection.immutable
import scala.util.Try
import scala.xml.Node

object posToCat{
  val map: Map[String,String]  = Map(
    "adj" -> "ap",  //93193
    "adv" -> "advp",  //50000
    "comp" -> "",  //32884
    "comparative" -> "",  //1585
    "det" -> "detp",  //144640
    "fixed" -> "",  //3848
    "name" -> "",  //83552
    "noun" -> "np",  //214104
    "num" -> "",  //17921
    "part" -> "",  //8034
    "pp" -> "advp",  //5751
    "prefix" -> "",  //694
    "prep" -> "",  //125638
    "pron" -> "np",  //28230
    "punct" -> "",  //120371
    "tag" -> "",  //1212
    "verb" -> "",  //133476
    "vg" -> "",  //29410
  )
}
// kijk naar https://grew.fr/gallery/flat/
case class AlpinoNode(s: AlpinoSentence, n: Node) {
  val id: String = (n \ "@id").text
  val word: String = (n \ "@word").text
  val pos: String = (n \ "@pos").text
  val xpos: String = (n \ "@postag").text
  val lemma: String = (n \ "@lemma").text
  val index: String = (n \ "@index").text
  val isWord: Boolean = word.nonEmpty // (n \\ "node").isEmpty
  val rel: String = (n \ "@rel").text
  val cat: String = (n \ "@cat").text

  lazy val catPlus = if (cat.nonEmpty) cat else {
    if (posToCat.map.contains(pos) && posToCat.map(pos).nonEmpty) posToCat.map(pos) else "_";
  }

  lazy val pathToDependencyHead: String = dependencyHead.map(h => s.joiningPath(this, h)).getOrElse("?") // headWithPath.path.map(_.rel).mkString("<")

  lazy val isNotADependencyLeaf: Boolean = s.words.exists(_.dependencyHead.map(_.id).contains(this.id))

  lazy val betterRel: String = s.conversionRules.betterRel(this)

  lazy val constituentsIAmTheHeadOf: Seq[AlpinoNode] = ancestor.filter(a => this.isWord && a.constituentHead.contains(this))
  lazy val relationsAboveMe: Seq[String] = constituentsIAmTheHeadOf.map(_.rel)
  lazy val constituentLabelsIAmTheHeadOf: String = constituentsIAmTheHeadOf.map(_.cat).mkString("_")

  // niet echt nodig
  def upwardPathTo(n: AlpinoNode, target: AlpinoNode):Seq[AlpinoNode]  = {
    if (n==target) Seq[AlpinoNode]() else  {
      Seq(n) ++  n.parent.map(upwardPathTo(_,target)).getOrElse(Seq())
    }
  }



  lazy val begin: String = (n \ "@begin").text
  val wordNumber: Int = Try (begin.toInt) match {case Success(x) => x ; case _ => 0 }

  lazy val beginBaseOne: String = Try(((n \ "@begin").text.toInt + 1).toString) match
  {
    case Success(x) => x ;
    case Failure(exception) =>
      Console.err.println(s"Error getting begin position of node $n")
      throw exception
  }

  lazy val parent: Option[AlpinoNode] = s.parentMap.get(id).flatMap(pid => s.nodeMap.get(pid))
  lazy val ancestor: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get +: parent.get.ancestor
  lazy val sibling: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get.children.filter(c => c != this)

  lazy val relsToTheTop: String = (this +: ancestor).map(_.rel).mkString(";")

  lazy val children: immutable.Seq[AlpinoNode] = (n \ "node").map(x => s.nodeMap((x \ "@id").text))
  lazy val depth: Int = if (parent.isEmpty) 0 else 1 + parent.get.depth;
  lazy val indent: String = "  " * depth
  lazy val descendant: immutable.Seq[AlpinoNode] = (n \\ "node").map(x => s.nodeMap((x \ "@id").text))
  lazy val wordsIn: Seq[AlpinoNode] = if (isWord) Seq() else descendant.filter(_.isWord).sortBy(_.wordNumber)
  lazy val text: String = wordsIn.map(_.word).mkString(" ")
  lazy val constituentHead: Option[AlpinoNode] = s.conversionRules.findConstituentHead(this)
  lazy val headWithPath: s.HeadWithPath = s.findHeadForWord(this)
  lazy val dependencyHead: Option[AlpinoNode] = headWithPath.node

  override def toString: String = s"${indent}Node(begin=$begin,rel=$rel,cat=$cat,word=$word,text=$text, children=${children.map(_.rel).mkString(";")})"
}
