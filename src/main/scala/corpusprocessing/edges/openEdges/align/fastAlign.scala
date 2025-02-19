package corpusprocessing.edges.openEdges.align

import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import corpusprocessing.edges.openEdges.Alignment

import scala.xml._
import utils.PostProcessXML

import sys.process._
import java.io.{File, PrintWriter}
import scala.collection.immutable

/*
Use fastAlign to add word alignment to verse-aligned bible XML
 */
case class fastAlign(bookDoc: Elem, unitElement: String="ab") {
  //val genesis = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/XMLConversie/alignment/en_1890_Darby.Gen--nl_1637_Staten.Gen.aligments.xml"


  lazy val allVerses = (bookDoc \\ unitElement)

  lazy val id2Verse: Map[String, Node] = allVerses.map(v => getId(v) -> v).toMap
  def getLang(w: Node) = w.attributes.filter(_.key == "lang").value.text


  def tokenizedText(v: Node) = v.descendant.filter(x => Set("w").contains(x.label)).map(_.text.replaceAll("\\s+", "")).toSeq // .mkString(" ")

  def createWordAlignmentLinks(alignment: Alignment) = {
    val pairs = makePairs(alignment)
    makeStandOffLinks(alignment,pairs)
  }

  def makePairs(alignment: Alignment)  = {

    // deze twee hoeven niet iedere keer

    val idsOfSimpleVerses = id2Verse.keySet.filter(alignment.isSimplyLinked(_))

    println(s"Simple verses: ${idsOfSimpleVerses.size}")

    val simpleSimplyLinking: NodeSeq = allVerses.filter(v => idsOfSimpleVerses.contains(getId(v)))  // idsOfSimpleVerses.map(id2Verse)

    val linkedVerses: immutable.Seq[(Node, Node)] = simpleSimplyLinking.map(v => v -> {
      val id = getId(v)
      val alignedId = alignment.aligned(id).head.xmlId
      val alignedVerse = id2Verse.getOrElse(alignedId, <nope/>)
      alignedVerse
    }).filter(_._2.label != "nope")

    println(s"all verses: ${allVerses.size}, Dutch simply linked: ${simpleSimplyLinking.size}")

    // write input file for fastAlign
    val alignmentFile = new PrintWriter("/tmp/bible.alignMe.txt")

    val tokenizedVerses = linkedVerses.map({ case (v1, v2) => {
      val t1 = tokenizedText(v1)
      val t2 = tokenizedText(v2)
      val ids1 = (v1 \\ "w").map(getId)
      val ids2 = (v2 \\ "w").map(getId)
      alignmentFile.println(s"${t1.mkString(" ")} ||| ${t2.mkString(" ")}")
      (t1, t2, ids1, ids2)
    }})

    val t1 = tokenizedVerses.map(_._1).toStream
    val t2 = tokenizedVerses.map(_._2).toStream
    val ids1 = tokenizedVerses.map(_._3).toStream
    val ids2 = tokenizedVerses.map(_._4).toStream

    alignmentFile.close()

    // run fastAlign

    val command = "fast_align -i /tmp/bible.alignMe.txt -N -d -o -v -I 10".split("\\s+").toSeq
    val lines: Stream[String] = command  lineStream;

    val alignedWordIds: Seq[(String, Seq[String])] = decodeAlignment.decodeAlignment(lines, ids1, ids2)

    println(s"Found ${alignedWordIds.size} word alignments in ${(bookDoc \\ "w").size} tokens")

    alignedWordIds
    // fast_align -i /tmp/bible.alignMe -N -d -o -v -I 10 > forward.align
  }



  def makeStandOffLinks(alignment: Alignment, alignedWordIds: Seq[(String, Seq[String])])  = {
    val wordLinks = alignedWordIds.flatMap({ case (l, r) => r.map(r1 => <link type="word-alignment" target={s"#$l #$r1"}/>) })
    val id = alignment.bible1 + "--" + alignment.bible2 + ".words"
    val alignmentLayer = <standOff type="word-alignment" xml:id={id}>
      {wordLinks}
    </standOff>



    // XML.save("/tmp/withWordLinks.xml", withLinks)
    alignmentLayer
  }


}
