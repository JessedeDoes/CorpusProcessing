package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.dependencyconversion.ConversionToFlatLassyRules
import utils.PostProcessXML

import java.io.{File, PrintWriter}
import scala.collection.immutable
import scala.xml._

// conversion SUD to UD cf: https://github.com/surfacesyntacticud/
// voorbeeld complexe nesting: https://paqu.let.rug.nl:8068/tree?db=lassysmall&arch=0&file=26887&yl=2&gr=3&ms=10,12,13
// /media/jesse/Data/Corpora/LassySmall/Treebank/WR-P-P-I-0000000198/WR-P-P-I-0000000198.p.12.s.6.xml
// http://svprre10.ivdnt.loc:8080/corpus-frontend/Alpino_dep_test/search/hits?first=0&number=20&patt=%3Cs%2F%3E+containing+%5Bother%3D%22.%2Asmain_du_du_du_whrel_top.%2A%22%5D&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D
object Logje {
  val pw = new PrintWriter("/tmp/logje.txt")
  def log(s: String): Unit = { pw.println(s); pw.flush() }
}
import Logje._





case class AlpinoSentence(alpino: Elem, external_id: Option[String] = None, external_source: Option[String] = None, conversionRules: ConversionRules = ConversionToFlatLassyRules) {

  val sentid: String = external_id.getOrElse((alpino \\ "sentence" \ "@sentid").text)
  val text: String = (alpino \\ "sentence").text
  val source : String = external_source.getOrElse("UNKNOWN")
  val nodeMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).map(n => n.id -> n).toMap
  val wordMap: Map[String, AlpinoNode] = (alpino \\ "node").map(n => AlpinoNode(this, n)).filter(_.isWord).map(n => n.beginBaseOne -> n).toMap
  lazy val words: Seq[AlpinoNode] = wordMap.values.toList.sortBy(_.wordNumber)

  val parentMap: Map[String, String] = nodeMap.flatMap(n => {
    val childIds = n._2.n.child.map(n => (n \ "@id").text).filter(_.nonEmpty)
    childIds.map(i => i -> n._2.id)
  }).toMap

  lazy val nodes: Seq[AlpinoNode] = nodeMap.values.toList.sortBy(x => 1000 * x.wordNumber + x.depth);
  lazy val topNode: Seq[AlpinoNode] = nodes.filter(_.parent.isEmpty)


  lazy val constituentsAndHeads: Seq[(AlpinoNode, Option[AlpinoNode])] = nodes.filter(!_.isWord).map(x =>  x -> ConversionToFlatLassyRules.findConstituentHead(x))
  def commonAncestor(n1: AlpinoNode, n2: AlpinoNode): Option[AlpinoNode]  = {
    if (n1 == n2) Some(n1) else if (n1.ancestor.contains(n2)) Some(n2) else if (n2.ancestor.contains(n1)) Some(n1)
    else if (n1.parent.nonEmpty && n2.parent.nonEmpty) commonAncestor(n1.parent.get, n2.parent.get) else None
  } // lijkt me niet goed als n1 al ancestor van n2 is

  def joiningPath(n1: AlpinoNode, n2:AlpinoNode): String = {
    val c = commonAncestor(n1,n2)
    if (c.nonEmpty) {
      val cp = c.get
      val p1 = (n1 +: n1.ancestor).takeWhile(x => x!= cp).map(n => s"[${n.rel}:${n.cat}]").mkString(">")
      val p2 = (n2 +: n2.ancestor).takeWhile(x => x!= cp).map(n => s"[${n.rel}:${n.cat}]").mkString("<")
      val center = s"[${cp.rel}:${cp.cat}]"
      s"{$p1 |$center| $p2}"
    } else ""
  }

  case class HeadWithPath(node: Option[AlpinoNode], path: Seq[AlpinoNode]) {
    override  def toString() = {

      node.toString + "  " + path.map(_.cat).mkString("|")
    }
  }

  def findHeadForWord(w: AlpinoNode, in: AlpinoNode): HeadWithPath = {

    val r: HeadWithPath = if (in.constituentHead.nonEmpty && in.constituentHead.get.id != w.id && in.constituentHead.get.begin != w.begin) {
      //if (w.word == "won") { Console.err.println(s"Yep ${in.constituentHead}")}
      HeadWithPath(in.constituentHead, Seq(in))
    } else if (in.parent.nonEmpty) {
      // if (w.word == "won") { Console.err.println(s"Moving to parent ${in.parent.get}")}
      val x = findHeadForWord(w, in.parent.get)
      x.copy(path = in +: x.path)
    }
    else HeadWithPath(None.asInstanceOf[Option[AlpinoNode]], Seq[AlpinoNode]())

    if (w.rel == "whd") {
      //println(this.text)
      // println(w + "==>"  + r)
    }
    r
  }

  def findHeadForWord(w: AlpinoNode) : HeadWithPath = {
    if (w.isWord && w.parent.nonEmpty) findHeadForWord(w, w.parent.get) else HeadWithPath(None,Seq())
  }

  lazy val input_transcript = (alpino \\ "comment").text.replaceAll("^.*?\\|", "")
  lazy val id = ((alpino \\ "sentence").head \ "@sentid").text
  lazy val alpinoTokens = (alpino \\ "node").filter(x => (x \ "@word").nonEmpty).map(x => AlpinoToken(x)).sortBy(_.begin)

  lazy val xml = <s xml:id={id}>
    {"\n"}{alpinoTokens.sortBy(_.begin).flatMap(t => Text("\t") :+ <w pos={t.postag} lemma={t.lemma}>
      {t.word}
    </w> +: Text("\n"))}
  </s>


  lazy val connlTokens = words.map(w =>  {
        val h = w.dependencyHead.map(x => (x.wordNumber+1).toString).getOrElse("0")
        val hw =  w.dependencyHead.map(x => x.word).getOrElse("_")
        val cat =  {
          val l = Map("CAT" -> w.constituentLabelsIAmTheHeadOf).filter({case (k,v) => v.nonEmpty})
          if (l.nonEmpty) l.map({case (k,v) => s"$k=$v"}).mkString("|") else "CATPLUS=" + w.catPlus
        }

        UdToken(
          ID=(w.wordNumber+1).toString,
          FORM=w.word,
          LEMMA = w.lemma,
          UPOS = w.pos,
          XPOS = w.xpos,
          FEATS = "_",
          HEAD = h,
          DEPREL = w.betterRel,
          DEPS=s"$h:$hw",
          MISC = cat // w.relsToTheTop
        )})

  lazy val dependencyParseIsValid: Boolean = {
    val udSentence = UdSentence(sentid, "Dutch", connlTokens)
    udSentence.isValid()
  }

  def toCONLL(): String = {
      val header = s"""# sent_id = $sentid
                     |# source = ${this.source}
                     |# text = ${this.text}""".stripMargin
       header + "\n" + connlTokens.map(_.toCONLL()).mkString("\n")
   }
}

