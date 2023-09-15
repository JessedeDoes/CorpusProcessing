package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

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

case class AlpinoToken(n: Node, id:Option[String] = None)  {
  val atts: Map[String, String] = n.attributes.map(n => n.key -> n.value.text).toMap
  lazy val lemma: String = atts.getOrElse("lemma","_")
  lazy val word: String = atts.getOrElse("word","_")
  lazy val postag: String = atts.getOrElse("postag","_")
  val begin: Int = atts("begin").toInt
}
case class AlpinoNode(s: AlpinoSentence, n: Node) {
  val id: String = (n \ "@id").text
  val word: String = (n \ "@word").text
  val pos: String = (n \ "@pos").text
  val xpos: String = (n \ "@postag").text
  val lemma: String =  (n \ "@lemma").text
  val isWord: Boolean = word.nonEmpty // (n \\ "node").isEmpty
  val rel: String = (n \ "@rel").text

  lazy val pathToDependencyHead: String = dependencyHead.map(h => s.joiningPath(this, h)).getOrElse("?")// headWithPath.path.map(_.rel).mkString("<")

  lazy val isNotADependencyLeaf: Boolean = s.words.exists(_.dependencyHead.map(_.id).contains(this.id))

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

  lazy val whoseHeadAmI: String = ancestor.filter(a => this.isWord && a.constituentHead.contains(this)).map(_.cat).mkString("_")

  val cat: String = (n \ "@cat").text

  val begin: String = (n \ "@begin").text
  val wordNumber: Int = begin.toInt
  val beginBaseOne: String = ((n \ "@begin").text.toInt + 1).toString
  lazy val parent: Option[AlpinoNode] = s.parentMap.get(id).flatMap(pid => s.nodeMap.get(pid))
  lazy val ancestor: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get +: parent.get.ancestor
  lazy val sibling: Seq[AlpinoNode] = if (parent.isEmpty) Seq() else parent.get.children.filter(c => c != this)

  lazy val relsToTheTop: String = (this +: ancestor).map(_.rel).mkString(";")

  lazy val children: immutable.Seq[AlpinoNode] = (n \ "node").map(x => s.nodeMap( (x \ "@id").text))
  lazy val depth: Int = if (parent.isEmpty) 0 else 1 + parent.get.depth;
  lazy val indent: String = "  " * depth
  lazy val descendant: immutable.Seq[AlpinoNode] = (n \\ "node").map(x => s.nodeMap( (x \ "@id").text))
  lazy val wordsIn: Seq[AlpinoNode] = if (isWord) Seq() else descendant.filter(_.isWord).sortBy(_.wordNumber)
  lazy val text: String = wordsIn.map(_.word).mkString(" ")
  lazy val constituentHead: Option[AlpinoNode] = s.findConstituentHead(this)
  lazy val headWithPath: s.HeadWithPath = s.findHeadForWord(this)
  lazy val dependencyHead: Option[AlpinoNode] = headWithPath.node
  override def toString: String = s"${indent}Node(begin=$begin,rel=$rel,cat=$cat,word=$word,text=$text, children=${children.map(_.rel).mkString(";")})"
}

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

  lazy val constituentsAndHeads: Seq[(AlpinoNode, Option[AlpinoNode])] = nodes.filter(!_.isWord).map(x =>  x -> findConstituentHead(x))
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
    val udSentence = UdSentence(sentid,"Dutch", connlTokens)
    udSentence.isValid()
  }

  def toCONLL(): String = {
      val header = s"""\n# sent_id = $sentid
                     |# source = ${this.source}
                     |# text = ${this.text}""".stripMargin
       header + "\n" + connlTokens.map(_.toCONLL()).mkString("\n")
   }
}

object testWithHeads  {
  import sys.process._
  val max= 3000 // Integer.MAX_VALUE // 100000
  val lassyAllAtHome = "/media/jesse/Data/Corpora/LassySmall/Treebank/"
  val lassyAtWork = "/mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/LassySmall/Treebank/"
  val lassy = List(lassyAllAtHome, lassyAtWork).find(x => new File(x).exists())


  // complex voorbeeld coindexeringen: https://paqu.let.rug.nl:8068/tree?db=lassysmall&names=true&mwu=false&arch=/net/corpora/paqu/lassyklein.dact&file=WR-P-P-I-0000000126.p.12.s.4.xml&global=true&marknodes=&ud1=&ud2=
  def transferInfo(x: Elem)  = {
    val leafs = (x \\ "node").filter(x => x.child.isEmpty)
    val groupedByBegin = leafs.groupBy(x => (x \ "@begin").text -> (x \ "@end").text)
    groupedByBegin.foreach({case (b,n) =>  if (n.size  > 1) {
      println("Colocated nodes!")
      n.foreach(println)
      // System.exit(1)
    }
    })
  }
  /*
  <node begin="49" end="50" genus="masc" getal="ev" id="88" index="3" lemma="hem" naamval="obl" pdtype="pron" persoon="3" pos="pron" postag="VNW(pers,pron,obl,vol,3,ev,masc)" pt="vnw" rel="obj2" root="hem" status="vol" vwtype="pers" word="hem"/>
  <node begin="49" end="50" id="93" index="3" rel="su"/>
   */


  val garrulous = false
  def main(args: Array[String])  = {

    val location = args.headOption.getOrElse(lassy.getOrElse("nope"))
    lazy val lines: Stream[String] = Seq("find", location, "-name", "*.xml") #| Seq("head", s"-$max") lineStream

    val conllOutput = new PrintWriter("/tmp/test.conll.txt")

    lines.foreach(l => {

      val sentence_id = new File(l).getName.replaceAll(".xml$", "")
      val x = XML.load(l)
      val sentence = AlpinoSentence(x, Some(sentence_id), Some(l))



      if (garrulous) {
        println(s"###############  $l #####################")
        sentence.constituentsAndHeads.foreach({case (x,y) => println(s"${x.indent} ${x.cat}/${x.rel} [${x.text}]  ----> ${y.map(x => x.word + ":" + x.betterRel + ":" + x.wordNumber).getOrElse("-")}")})
        println(s"### pure dependencies for ${sentence.sentid} ###")
        println(sentence.toCONLL())
      }

      if (sentence.dependencyParseIsValid) {
        conllOutput.println(sentence.toCONLL())
        conllOutput.flush()
      } else {
        Console.err.println(s"Bummer, invalid dependency parse for ${sentence.sentid} !")
      }
      // transferInfo(x)
    })
  }
}
