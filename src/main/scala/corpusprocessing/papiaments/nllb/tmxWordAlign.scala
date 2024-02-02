package corpusprocessing.papiaments.nllb

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.xml._
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import scala.collection.immutable
import scala.sys.process._
import scala.xml._
import java.io._
case class tmxWordAlign(tmxDoc: Elem) {
  val genesis = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/XMLConversie/alignment/en_1890_Darby.Gen--nl_1637_Staten.Gen.aligments.xml"


  lazy val allVerses = (tmxDoc \\ "ab")
  lazy val id2Verse: Map[String, Node] = allVerses.map(v => getId(v) -> v).toMap

  def getLang(w: Node) = w.attributes.filter(_.key == "lang").value.text


  def tokenizedText(v: Node) = v.descendant.filter(x => Set("w", "pc").contains(x.label)).map(_.text.replaceAll("\\s+", "")).toSeq // .mkString(" ")

  def addWordAlignment() = {

    println("Youp!")
    // deze twee hoeven niet iedere keer

    val linkedVerses: immutable.Seq[(Node, Node)] = (tmxDoc \\ "tu").map(tu => {
      val tuvs = (tu \ "tuv")
      if (tuvs.size >= 0)
        tuvs(0) -> tuvs(1)
      else
          <nope/> -> <nope/>
    }).filter({ case (x, y) => x.label != "nope" })


    // write input file for fastAlign
    val alignmentFile = new PrintWriter("/tmp/bible.alignMe.txt")

    val tokenizedVerses: immutable.Seq[(immutable.Seq[String], immutable.Seq[String], immutable.Seq[String], immutable.Seq[String])] =
      linkedVerses.map({ case (v1, v2) => {
        val t1 = tokenizedText(v1)
        val t2 = tokenizedText(v2)
        val ids1 = (v1 \\ "w").map(getId)
        val ids2 = (v2 \\ "w").map(getId)
        alignmentFile.println(s"${t1.mkString(" ")} ||| ${t2.mkString(" ")}")
        (t1, t2, ids1, ids2)
      }
      })

    val t1 = tokenizedVerses.map(_._1).toStream
    val t2 = tokenizedVerses.map(_._2).toStream
    val ids1 = tokenizedVerses.map(_._3).toStream
    val ids2 = tokenizedVerses.map(_._4).toStream

    alignmentFile.close()

    // run fastAlign

    val command = "fast_align -i /tmp/bible.alignMe.txt -N -d -o -v -I 10".split("\\s+").toSeq
    val lines: Stream[String] = command lineStream;

    val allPairs = lines.zip(linkedVerses).flatMap({
      case (line, (v1, v2)) => {

        val w1 = (v1 \\ "w").map(_.text)
        val w2 = (v2 \\ "w").map(_.text)
        val pairings = line.trim.split("\\s+").filter(_.contains("-")).map(p => {
          //println(p)
          val lr = p.split("-")
          lr(0).toInt -> lr(1).toInt
        })

        val leftCounts = pairings.groupBy(_._1).mapValues(_.size)
        val rightCounts = pairings.groupBy(_._2).mapValues(_.size)
        val simplePairings = pairings.filter({ case (a, b) => leftCounts(a) == 1 && rightCounts(b) == 1 })
        val pairs = simplePairings.map({
          case (i, j) if (i < w1.size && j < w2.size) => w1(i) -> w2(j)
          case (i, j) => s"_nope$i" -> s"_nope$j"
        }).toList
        // println(s"$line ${v1.text} $pairs")
        pairs.filter({ case (a, b) => !a.startsWith("_nope") })
      }
    })

    val lexicon = allPairs.groupBy(identity).mapValues(_.size)
    val pw = new PrintWriter("/tmp/pairs.out")
    lexicon.toList.sortBy(-1 * _._2).foreach({ case ((a, b), c) => pw.println(s"$a\t$b\t$c") })
    pw.close()











    /*
    //lines.foreach(println)
    val alignedWordIds: Seq[(String, Seq[String])] = decodeAlignment.decodeAlignment(lines, ids1, ids2)

    //alignedWordIds.foreach(println)

    println(s"Found ${alignedWordIds.size} word alignments in ${(tmxDoc \\ "w").size} tokens")

    val wordLinks = alignedWordIds.flatMap({case (l,r) => r.map(r1 => <link type="word-alignment" target={s"#$l #$r1"}/>)})
   */


    // fast_align -i /tmp/bible.alignMe -N -d -o -v -I 10 > forward.align
  }

}

object testje {
  val stukje0 = "/mnt/Projecten/Papiaments/Corpusdata/NLLB/stukje.tok.tmx.gz"
  val stukje ="/tmp/stukje.tok.tmx.gz"
  val streampje = new GZIPInputStream(new FileInputStream(stukje))
  def main(args: Array[String])  = {
    tmxWordAlign(XML.load(streampje)).addWordAlignment()
  }
}