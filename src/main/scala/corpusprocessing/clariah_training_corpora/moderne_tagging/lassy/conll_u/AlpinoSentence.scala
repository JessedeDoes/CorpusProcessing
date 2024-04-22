package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

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





case class AlpinoSentence(alpino: Elem, external_id: Option[String] = None, external_source: Option[String] = None) {

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


  lazy val constituentsAndHeads: Seq[(AlpinoNode, Option[AlpinoNode])] = nodes.filter(!_.isWord).map(x =>  x -> SpecificConversionRules.findConstituentHead(x))
  def commonAncestor(n1: AlpinoNode, n2: AlpinoNode): Option[AlpinoNode]  = {
    if (n1 == n2) Some(n1) else if (n1.parent.contains(n2)) Some(n2) else if (n2.parent.contains(n1)) Some(n1)
    else if (n1.parent.nonEmpty && n2.parent.nonEmpty) commonAncestor(n1.parent.get, n2.parent.get) else None
  }

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
          MISC = Map("CAT" -> w.whoseHeadAmI).filter({case (k,v) => v.nonEmpty}).map({case (k,v) => s"$k=$v"}).mkString("|") // w.relsToTheTop
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

// old stuff before refactor into specific rules object

/*
lazy val betterRel: String = {
  lazy val up = parent.get.betterRel

  val r0 = rel match {
    case _ if parent.isEmpty => rel
    case _ if (pos=="punct") => "punct"
    case _ if isWord && dependencyHead.isEmpty => "root"

    // nonfirst part of cooordination or multiword keeps its rel

    case "cnj" if dependencyHead.nonEmpty && dependencyHead.head.rel == "cnj" => this.rel // Pas Op deugt deze regel wel?
    case "mwp" if dependencyHead.nonEmpty && dependencyHead.head.betterRel == "mwp" => this.rel
  /*
  lazy val betterRel: String = {
    lazy val up = parent.get.betterRel

    val r0 = rel match {
      case _ if parent.isEmpty => rel
      case _ if (pos=="punct") => "punct"
      case _ if isWord && dependencyHead.isEmpty => "root"

      // nonfirst part of cooordination or multiword keeps its rel

      case "cnj" if dependencyHead.nonEmpty && dependencyHead.head.rel == "cnj" => this.rel // Pas Op deugt deze regel wel?
      case "mwp" if dependencyHead.nonEmpty && dependencyHead.head.betterRel == "mwp" => this.rel


      case "cnj" if  !sibling.exists(x => x.rel == "cnj" && x.wordNumber < this.wordNumber) => up
      //case "cnj" if parent.exists(_.cat=="conj" && !(parent.get.children.exists(_.wordNumber < this.wordNumber))) => s"Tja!: ${parent.get.children.map(_.rel).mkString("|")}"
      case "mwp" if  !sibling.exists(x => x.rel == "mwp" && x.wordNumber < this.wordNumber) => up

      case "hd" => up
      case "body" => up


      case "nucl" => up
      case _ => rel
    }
    // if (dependencyHead.isEmpty || r0=="0") "root" else if (pos == "punct") "punct" else r0
    r0
  }
   */

    case "cnj" if  !sibling.exists(x => x.rel == "cnj" && x.wordNumber < this.wordNumber) => up
    //case "cnj" if parent.exists(_.cat=="conj" && !(parent.get.children.exists(_.wordNumber < this.wordNumber))) => s"Tja!: ${parent.get.children.map(_.rel).mkString("|")}"
    case "mwp" if  !sibling.exists(x => x.rel == "mwp" && x.wordNumber < this.wordNumber) => up

    case "hd" => up
    case "body" => up


    case "nucl" => up
    case _ => rel
  }
  // if (dependencyHead.isEmpty || r0=="0") "root" else if (pos == "punct") "punct" else r0
  r0
}
 */

/*
def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false):Option[AlpinoNode]  = {
  if (n.isWord) (if (allowLeaf) Some(n) else None) else {

    if (!n.children.exists(x => x.rel == "hd")) {
      log(s"Exocentric node: ${n.cat}:${n.rel} (${n.children.map(c => s"${c.cat}:${c.rel}").mkString(",")}) ")
    }

    def searchIn(relName: String)  = n.children.find(x => x.rel == relName).flatMap(x => findConstituentHead(x, true))

    val immediateHead: Option[AlpinoNode] = n.children.find(x => x.rel == "hd" && x.isWord)

    val intermediateHead : Option[AlpinoNode] = searchIn("hd") //  n.children.filter(x => x.rel == "hd").headOption.flatMap(x => findConstituentHead(x))

    val usingCmp: Option[AlpinoNode] = searchIn("cmp")
    val usingBody: Option[AlpinoNode] = searchIn("body")

    val usingMwp: Option[AlpinoNode] = n.children.find(_.rel == "mwp").filter(_.isWord)
    val usingCnj : Option[AlpinoNode] = searchIn("cnj")// dit werkt dus niet altijd .....
    val usingNucl: Option[AlpinoNode] = searchIn("nucl")
    val usingDp:  Option[AlpinoNode] = searchIn("dp")

    val gedoeStreepjes =  n.children.find(x => x.rel == "--" && !x.isWord).flatMap(x => findConstituentHead(x))

    (immediateHead.toList
      ++ intermediateHead.toList
      ++ usingCmp.toList
      ++ usingBody.toList
      ++ usingMwp.toList
      ++ gedoeStreepjes.toList
      ++ usingCnj.toList
      ++ usingNucl.toList
      ++ usingDp.toList).headOption
  }
}
 */