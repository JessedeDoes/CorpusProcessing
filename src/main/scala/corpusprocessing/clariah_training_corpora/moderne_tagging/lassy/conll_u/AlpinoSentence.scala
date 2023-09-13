package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import java.io.{File, PrintWriter}
import scala.collection.immutable
import scala.xml._

object Logje {
  val pw = new PrintWriter("/tmp/logje.txt")
  def log(s: String) = { pw.println(s); pw.flush() }
}
import Logje._

case class AlpinoToken(n: Node, id:Option[String] = None)  {
  val atts = n.attributes.map(n => n.key -> n.value.text).toMap
  lazy val lemma = atts.getOrElse("lemma","_")
  lazy val word = atts.getOrElse("word","_")
  lazy val postag = atts.getOrElse("postag","_")
  val begin = atts("begin").toInt
}
case class AlpinoNode(s: AlpinoSentence, n: Node) {
  val id = (n \ "@id").text
  val word = (n \ "@word").text
  val pos = (n \ "@pos").text
  val xpos = (n \ "@postag").text
  val lemma =  (n \ "@lemma").text
  val isLeaf = word.nonEmpty // (n \\ "node").isEmpty
  val rel = (n \ "@rel").text

  lazy val pathToDependencyHead = dependencyHead.map(h => s.joiningPath(this, h)).getOrElse("?")// headWithPath.path.map(_.rel).mkString("<")

  lazy val isNotADependencyLeaf = s.words.exists(_.dependencyHead.map(_.id) == Some(this.id))

  lazy val betterRel: String = {
    lazy val up = parent.get.betterRel

    val r0 = rel match {
      case _ if parent.isEmpty => rel
      case "cnj" if dependencyHead.nonEmpty && dependencyHead.head.rel == "cnj" => this.rel
      case "mwp" if dependencyHead.nonEmpty && dependencyHead.head.rel == "mwp" => this.rel
      case "cnj" if  !sibling.exists(x => x.rel == "cnj" && x.wordNumber < this.wordNumber) => up
      case "hd" => up
      case "body" => up
      case "mwp" => up
      case "nucl" => up
      case _ => rel
    }
    if (r0=="0") "root" else if (pos == "punct") "punct" else r0
  }


  val cat = (n \ "@cat").text

  val begin = (n \ "@begin").text
  val wordNumber = begin.toInt
  val beginBaseOne = ((n \ "@begin").text.toInt + 1).toString
  lazy val parent: Option[AlpinoNode] = s.parentMap.get(id).flatMap(pid => s.nodeMap.get(pid))
  lazy val ancestor: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get +: parent.get.ancestor
  lazy val sibling: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get.children.filter(c => c != this)
  lazy val relsToTheTop = (this +: ancestor).map(_.rel).mkString(";")

  lazy val children: immutable.Seq[AlpinoNode] = (n \ "node").map(x => s.nodeMap( (x \ "@id").text))
  lazy val depth: Int = if (parent.isEmpty) 0 else 1 + parent.get.depth;
  lazy val indent = "  " * depth
  lazy val descendant = (n \\ "node").map(x => s.nodeMap( (x \ "@id").text))
  lazy val wordsIn: Seq[AlpinoNode] = if (isLeaf) Seq() else descendant.filter(_.isLeaf).sortBy(_.wordNumber)
  lazy val text = wordsIn.map(_.word).mkString(" ")
  lazy val constituentHead: Option[AlpinoNode] = s.findConstituentHead(this)
  lazy val headWithPath = s.findHeadForWord(this)
  lazy val dependencyHead = headWithPath.node
  override def toString: String = s"${indent}Node(begin=$begin,rel=$rel,cat=$cat,word=$word,text=$text, children=${children.map(_.rel).mkString(";")})"
}

case class AlpinoSentence(alpino: Elem) {

  val sentid = (alpino \\ "sentence" \ "@sentid").text
  val text = (alpino \\ "sentence").text

  val nodeMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).map(n => n.id -> n).toMap
  val wordMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).filter(_.isLeaf).map(n => n.beginBaseOne -> n).toMap
  lazy val words = wordMap.values.toList.sortBy(_.wordNumber)
  // println(wordMap.keySet)
  val parentMap = nodeMap.flatMap(n => {
    val childIds = n._2.n.child.map(n => (n \ "@id").text).filter(_.nonEmpty)
    childIds.map(i => i -> n._2.id)
  }).toMap

  lazy val nodes = nodeMap.values.toList.sortBy(x => 1000 * x.wordNumber + x.depth);
  lazy val topNode = nodes.filter(_.parent.isEmpty)

  def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false):Option[AlpinoNode]  = {
    if (n.isLeaf) (if (allowLeaf) Some(n) else None) else {
      val immediateHead: Option[AlpinoNode] = n.children.filter(x => x.rel == "hd" && x.isLeaf).headOption

      val intermediateHead : Option[AlpinoNode] = n.children.filter(x => x.rel == "hd").headOption.flatMap(x => findConstituentHead(x))
      if (n.children.filter(x => x.rel == "hd").isEmpty) {
        log(s"Exocentric node: ${n.cat}:${n.rel} (${n.children.map(c => s"${c.cat}:${c.rel}").mkString(",")}) ")
      }
      val usingCmp: Option[AlpinoNode] = n.children.find(_.rel == "cmp").flatMap(x => findConstituentHead(x))
      val usingBody: Option[AlpinoNode] = n.children.find(_.rel == "body").flatMap(x => findConstituentHead(x))

      val usingMwp: Option[AlpinoNode] = n.children.find(_.rel == "mwp").filter(_.isLeaf)
      val usingCnj : Option[AlpinoNode] = n.children.find(_.rel == "cnj").flatMap(x => findConstituentHead(x, true))// dit werkt dus niet altijd .....
      if (usingCnj.nonEmpty) {
        // Console.err.println(s"$n -> $usingCnj")
        // System.exit(1)
      }
      val usingNucl: Option[AlpinoNode] = n.children.find(_.rel == "nucl").flatMap(x => findConstituentHead(x))
      val gedoeStreepjes =  n.children.find(x => x.rel == "--" && !x.isLeaf).flatMap(x => findConstituentHead(x))
      (immediateHead.toList ++  intermediateHead.toList ++ usingBody.toList  ++ usingMwp.toList ++ gedoeStreepjes.toList ++ usingCnj.toList ++ usingNucl.toList).headOption
    }
  }


  def commonAncestor(n1: AlpinoNode, n2: AlpinoNode): Option[AlpinoNode]  = {
    if (n1 == n2) Some(n1) else if (n1.parent.contains(n2)) Some(n2) else if (n2.parent.contains(n1)) Some(n1)
    else if (n1.parent.nonEmpty && n2.parent.nonEmpty) commonAncestor(n1.parent.get, n2.parent.get) else None
  }

  def joiningPath(n1: AlpinoNode, n2:AlpinoNode)  = {
    val c = commonAncestor(n1,n2)
    if (c.nonEmpty) {
      val cp = c.get
      val p1 = (n1 +: n1.ancestor).takeWhile(x => x!= cp).map(n => s"[${n.rel}:${n.cat}]").mkString(">")
      val p2 = (n2 +: n2.ancestor).takeWhile(x => x!= cp).map(n => s"[${n.rel}:${n.cat}]").mkString("<")
      val center = s"[${cp.rel}:${cp.cat}]"
      s"{$p1 |$center| $p2}"
    } else ""
  }

  case class HeadWithPath(node: Option[AlpinoNode], path: Seq[AlpinoNode])

  def findHeadForWord(w: AlpinoNode, in: AlpinoNode):HeadWithPath = {
     if (in.constituentHead.nonEmpty && in.constituentHead.get.id != w.id && in.constituentHead.get.begin != w.begin) {
       //if (w.word == "won") { Console.err.println(s"Yep ${in.constituentHead}")}
       HeadWithPath(in.constituentHead, Seq(in))
     } else if (in.parent.nonEmpty) {
       // if (w.word == "won") { Console.err.println(s"Moving to parent ${in.parent.get}")}
       val x = findHeadForWord(w, in.parent.get)
       x.copy(path = in +: x.path)
     }
     else HeadWithPath(None.asInstanceOf[Option[AlpinoNode]], Seq[AlpinoNode]())
  }

  def findHeadForWord(w: AlpinoNode) : HeadWithPath = {
    if (w.isLeaf && w.parent.nonEmpty) findHeadForWord(w, w.parent.get) else HeadWithPath(None,Seq())
  }

  lazy val input_transcript = (alpino \\ "comment").text.replaceAll("^.*?\\|", "")
  lazy val id = ((alpino \\ "sentence").head \ "@sentid").text
  lazy val alpinoTokens = (alpino \\ "node").filter(x => (x \ "@word").nonEmpty).map(x => AlpinoToken(x)).sortBy(_.begin)
  lazy val xml = <s xml:id={id}>
    {"\n"}{alpinoTokens.sortBy(_.begin).flatMap(t => Text("\t") :+ <w pos={t.postag} lemma={t.lemma}>
      {t.word}
    </w> +: Text("\n"))}
  </s>
}

object testWithHeads  {
  import sys.process._
  val max= 1000
  val lassyAllAtHome = "/media/jesse/Data/Corpora/LassySmall/Treebank/"
  lazy val lines: Stream[String] = Seq("find", lassyAllAtHome, "-name", "*.xml") #| Seq("head", s"-$max") lineStream

  def main(args: Array[String])  = {

    val out = new PrintWriter("/tmp/test.conll.txt")
    lines.foreach(l => {
      val sentence_id = new File(l).getName.replaceAll(".xml$", "")
      println(s"###############  $l #####################")
      val x = XML.load(l)
      val sentence = AlpinoSentence(x)
      val headjes = sentence.nodes.filter(!_.isLeaf).map(x =>  x -> sentence.findConstituentHead(x))

      headjes.foreach({case (x,y) => println(s"${x.indent} ${x.cat}/${x.rel} [${x.text}]  ----> ${y.map(x => x.word + ":" + x.betterRel + ":" + x.wordNumber).getOrElse("-")}")})
      println(s"### pure dependencies ${sentence.sentid} ###")
      out.println(s"""\n# source = $l
                     |# sent_id = $sentence_id
                     |# text = ${sentence.text}""".stripMargin)

      sentence.words.foreach(w =>  {
        val h = w.dependencyHead.map(x => (x.wordNumber+1).toString).getOrElse("0")
        val hw =  w.dependencyHead.map(x => x.word).getOrElse("_")
        lazy val udToken = UdToken(
          ID=(w.wordNumber+1).toString,
          FORM=w.word,
          LEMMA = w.lemma,
          UPOS = w.pos,
          XPOS = w.xpos,
          FEATS = "_",
          HEAD = h,
          DEPREL = w.betterRel,
          DEPS=s"$h:$hw",
          MISC = w.relsToTheTop
        )
        println(udToken.toCONLL())
        out.println(udToken.toCONLL())
        out.flush()
        // println(s"${w.begin} ${w.word} ${w.betterRel}  ${w.dependencyHead.map(_.begin).getOrElse("-")} ${w.dependencyHead.map(_.word).getOrElse("-")}")
      })
    })
  }
}
