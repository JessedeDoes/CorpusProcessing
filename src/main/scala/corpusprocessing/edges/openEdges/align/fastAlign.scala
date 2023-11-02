package corpusprocessing.edges.openEdges.align

import corpusprocessing.clariah_training_corpora.fixTokenization.getId

import scala.xml._
import utils.PostProcessXML
import sys.process._

import java.io.{File, PrintWriter}
import scala.collection.immutable

/*
Use fastAlign to add word alignment to verse-aligned bible XML
 */
object fastAlign {
  val genesis = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/XMLConversie/alignment/en_1890_Darby.Gen--nl_1637_Staten.Gen.aligments.xml"

  def getLang(w: Node) = w.attributes.filter(_.key == "lang").value.text

  def setAttribute(e: Elem, prefix: String, name: String, value: String)  = {
    val a = e.attributes.filter(_.key != name).append(new PrefixedAttribute(prefix, name, value, Null))
    e.copy(attributes = a)
  }
  def tokenizedText(v: Node) = v.descendant.filter(x => Set("w").contains(x.label)).map(_.text.replaceAll("\\s+", "")).toSeq // .mkString(" ")


  def makeIdsGloballyUnique(d: Elem)  = {
    PostProcessXML.updateElement(d, _.label == "ab", x => PostProcessXML.updateElement(x, _.label == "w", y => setAttribute(y, "xml", "id", getId(x)  + "." + getId(y))))
  }

  def loadFile(fileName: String) = {
    val d = XML.load(fileName)

    val d1 = PostProcessXML.updateElement(d, _.label == "include", x => {
      val href = (x \ "@href").text
      val dir = new File(fileName).getParentFile.getCanonicalPath
      val included = XML.load(dir + "/" + href)
      included
    })

    val d2 = makeIdsGloballyUnique(d1)

    val allVerses = (d2 \\ "ab")
    val simpleVerses = allVerses.filter(w => (w \ "@corresp").nonEmpty && !(w \ "@corresp").text.contains(" "))
    val idsOfSimpleVerses = simpleVerses.map(w => getId(w)).toSet
    val simpleVerseMap = simpleVerses.map(v => getId(v) -> v).toMap
    val simpleSimplyLinking = simpleVerses.filter(w => getLang(w) == "nl" && idsOfSimpleVerses.contains((w \ "@corresp").text.replaceAll("#", "")))
    val linkedVerses = simpleSimplyLinking.map(v => v -> simpleVerseMap((v \ "@corresp").text.replaceAll("#", "")))

    println(linkedVerses.size)

    val alignmentFile = new PrintWriter("/tmp/bible.alignMe.txt")
    val tokenizedVerses = linkedVerses.map({ case (v1, v2) => {
      val t1 = tokenizedText(v1)
      val t2 = tokenizedText(v2)
      val ids1 = (v1 \\ "w").map(getId)
      val ids2 = (v2 \\ "w").map(getId)
      alignmentFile.println(s"${t1.mkString(" ")} ||| ${t2.mkString(" ")}")

      /* println("         ")
      println(v1.text.replaceAll("\\s+", " "))
      println(v2.text.replaceAll("\\s+", " ")) */
      (t1, t2, ids1, ids2)
    }
    })

    val t1 = tokenizedVerses.map(_._1).toStream
    val t2 = tokenizedVerses.map(_._2).toStream
    val ids1 = tokenizedVerses.map(_._3).toStream
    val ids2 = tokenizedVerses.map(_._4).toStream

    alignmentFile.close()
    val command = "fast_align -i /tmp/bible.alignMe.txt -N -d -o -v -I 10".split("\\s+").toSeq
    val lines: Stream[String] = command  lineStream;
    val alignedWordIds: Seq[(String, Seq[String])] = decodeAlignment.decodeAlignment(lines, ids1, ids2)
    alignedWordIds.foreach(println)
    val wordLinks = alignedWordIds.flatMap({case (l,r) => r.map(r1 => <link type="word-alignment" target={s"#$l #$r1"}/>)})
    val linkGrp = <linkGrp type="word-alignment">{wordLinks}</linkGrp>
    val withLinks = PostProcessXML.updateElement(d2, _.label == "standOff", x => x.copy(child = x.child ++ linkGrp))

    XML.save("/tmp/withWordLinks.xml", withLinks)
    // fast_align -i /tmp/bible.alignMe -N -d -o -v -I 10 > forward.align
  }

  def main(args: Array[String]) = {
    loadFile(genesis)
  }
}
