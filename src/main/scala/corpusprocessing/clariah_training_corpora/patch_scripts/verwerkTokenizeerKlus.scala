package corpusprocessing.clariah_training_corpora.patch_scripts

import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.{getId, groupBy}
import utils.{PostProcessXML, ProcessFolder}

import java.io.File
import scala.collection._
import scala.xml._

case class Token(n: Int, word: String, lemma: String, pos: String, tag: String, id: String, docId: String, pid: Int, action: String) {
  lazy val hasAction = action.size > 4
}

object verwerkTokenizeerKlus {
  val bestandje = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Klus/tokens.txt"
  val outDir = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/PostProcessed/"

  val handled: mutable.Set[String] = mutable.Set()

  lazy val tokens = io.Source.fromFile(bestandje)
    .getLines().toSeq
    .map(l => l.split("\\t",-1))
    .map(a => Token(a(0).toInt, a(1), a(2), a(3), a(4), a(5), a(6), a(7).toInt, a(8)))

  lazy val tokensGrouped = groupTokens(tokens)
  lazy val t2g: Map[String, String] = tokensGrouped.flatMap(g => g.map(t => t.id -> g.head.id)).toMap
  lazy val t2a = tokens.map(t => t.id -> t.action).toMap
  lazy val t2w = tokens.map(t => t.id -> t.word).toMap
  lazy val tid2t = tokens.map(t => t.id -> t).toMap

  lazy val nontrivialWordIds = tokens.filter(_.hasAction).map(_.id).toSet

  def getGroup(n: Node): String = t2g.getOrElse(fixTokenization.getId(n), {
    Console.err.println(s"Whyyyyyyy $n")
    System.exit(1)
    fixTokenization.getId(n)} )

  def fix(s: Elem) = {
    val groupedChild = groupBy(s.child.filter(_.isInstanceOf[Elem]), getGroup, g => if (g.size==0) g.head else <group>{g}</group>)
    s.copy(child=groupedChild)
  }

  def peuterLosjes(w: Elem, splitOn: String="_"): Seq[Node]  =  {
    val pos = (w \ "@pos").text
    val txt = w.text

    val txt1 = t2w.getOrElse(getId(w), "hadjmemaar")

    if (!t2w.contains(getId(w))) {
      Console.err.println(s"Token $w not found in map. Kan niet he!")
      System.exit(1)
    }
    val pat = "NaTokenizeerKlus"

    //if (pat == 2 && txt1.contains("__")) Console.err.println("p" + pat + ":" + txt1  + " "  + w)

    if (txt1.contains(splitOn)) {
      val posParts = pos.split("\\+").toList
      val lemParts = (w \ "@lemma").text.split("\\+").toList
      val wordParts = txt1.split(splitOn).toList
      val canSplit = Set(posParts, lemParts,wordParts).map(_.size).size == 1

      val (posPartsF, lemPartsF) = if (posParts.size > 1 &&  !canSplit && posParts.size == 3 && lemParts.size == 3 && wordParts.size == 2)
      {
        Console.err.println(s"!!!! Cannot split $pat $txt1  in $w, but will try...")
        val posParts_new = Seq(posParts(0), s"${posParts(1)}+${posParts(2)}")
        val lemParts_new = Seq(lemParts(0), s"${lemParts(1)}+${lemParts(2)}")
        (posParts_new, lemParts_new)
      } else (posParts, lemParts)

      val replacement = if (posParts.size > 1 &&  Set(posPartsF, lemPartsF,wordParts).map(_.size).size == 1) {
        posPartsF.indices.toList.map(i => {
          val id = getId(w) + "." + i
          <w xml:id={id} resp={s"postprocess-2:$pat"} type={(w \ "@type").text} n={(w \ "@n").text} pos={posPartsF(i)} lemma={lemPartsF(i)} ctag={(w \ "@ctag").text}>{wordParts(i)}</w>}
        ).map(w => if (w.text.matches("^[,.:?]$")) w.copy(label="pc") else w)
      } else Seq(w)

      if (w != replacement) {
        if (false) println(
          s"""\n#### $txt1 $pat
             |Vervang ${w}
             |Door:
             |${replacement.mkString("\n")}""".stripMargin)
      } else {
        Console.err.println(s"#!#!#!#!#!#!#!#!#!# Geen splitsing voor $txt1 in $w")
      }
      replacement
    } else w
  }

  def fixDocje(doc: Elem) = {
    val d0 = PostProcessXML.updateElement(doc, _.label=="s" , fix)

    val d1 = PostProcessXML.updateElement5(d0, _.label=="group", g => {
      if (g.child.size >=2) {
        val w0 = g.child.head.asInstanceOf[Elem]
        val allText = g.map(_.text)
        g.child.foreach(w => handled.add(getId(w)))
        Console.err.println(s"Lumping $g")
        w0.copy(
          attributes = w0.attributes.filter(_.key != "type")
            .append(new UnprefixedAttribute("org",  allText.mkString("|"), Null))
            .append(new UnprefixedAttribute("resp", "plakkenMaar", Null)),
          child = Text(allText.mkString("")))
      } else g.child
    }).asInstanceOf[Elem]

    val d2 = PostProcessXML.updateElement5(d1, n => n.label=="w" && t2a.contains(getId(n)) && t2a(getId(n)) == "splits", n => peuterLosjes(n,"_")).asInstanceOf[Elem]
    val d3 = PostProcessXML.updateElement5(d2, n => Set("pc", "w").contains(n.label) && t2a.contains(getId(n)) && t2a(getId(n)) == "verwijder", n =>  {
      handled.add(getId(n))
      Seq()}
    )
    d3.asInstanceOf[Elem]
  }

  def groupTokens(l: Seq[Token]) = {
    val b0: Seq[Seq[Token]] = Seq()

    l.zipWithIndex.foldLeft(b0)({case (b, (t,i)) => {
      val action = t.action
      val prevaction = if (i > 0) l(i-1).action
      if (i == 0)
        Seq(Seq(t))
      else {
        val prevtoken = l(i-1)
        if (prevtoken.docId == t.docId && (action == "aanvorige" || prevaction == "aanvolgende"))  {
             //Console.err.println(s"$prevtoken $t")
            b.dropRight(1) :+ (b.last :+ t)
        } else b :+ Seq(t)
      }
    }})
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(fixTokenization.fixedDir), new File(outDir), {case (i,o) =>
      if (i.endsWith(".xml")) {
        val inDoc = XML.load(i)
        val outDoc = fixDocje(inDoc)
        val outTSV = o.replaceAll(".xml$", ".tsv")
        XML.save(o, outDoc, "UTF-8")
        fixTokenization.makeTSV(outDoc, outTSV)
      }
    })
    val missed = (nontrivialWordIds.toSet diff handled.toSet).map(tid2t)
    println("### Niet gedaan:")

    missed.foreach(println)

    //println("#### Wel gedaan:")
    //handled.foreach(println)
    //val x = fixDocje(bartje)
  }
}

/*
741     gheen-  degene  PD(type=d-p,position=free)      w       enge022vand01_01.TEI.2.text.body.div.lg.7633.s.4.w.305  enge022vand01_01        742     aanvolgende
742     re      degene  PD(type=d-p,position=free)      w       enge022vand01_01.TEI.2.text.body.div.lg.7633.s.4.w.306  enge022vand01_01        743     aanvorige

 */