package corpusprocessing.papiaments.nllb

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.xml._
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import scala.collection.{Seq, immutable}
import scala.sys.process._
import scala.xml._
import java.io._

// https://github.com/sillsdev/giza-py
case class Line(w1: Seq[String], w2:Seq[String], id1:Seq[String] = Seq(), id2: Seq[String] = Seq())
object wordAlign {

  def reassembleLines(lines: Stream[String], pharaohLines: Stream[String], separator: String="\\|\\|\\|"): Unit = {
   val tokenizedSegments = lines.map(line => {
     val (l, r) = line.split(separator) match {
       case Array(a: String,b: String) =>

         (a.trim.split("\\s+"),b.split("\\s+"))
     }
     val x = Line(l.filter(_.nonEmpty),r.filter(_.nonEmpty))
     // println(x)
     x
    })
    reassemble(tokenizedSegments, pharaohLines)
  }

  def reassembleText(textFile: String, pharaohFile: String, separator: String="\\|\\|\\|"): Unit  = {
    reassembleLines(io.Source.fromFile(textFile).getLines().toStream, io.Source.fromFile(pharaohFile).getLines().toStream, separator)
  }

  def reassemble(tokenizedSegments: Seq[Line], pharaohLines: Stream[String]): Unit = {
    val allPairs = pharaohLines.zip(tokenizedSegments).flatMap({
      case (line, Line(w1, w2, id1, id2)) => {
        // println(s"$line $w1 $w2")
        val pairings: Array[(Int, Int)] = line.trim.split("\\s+").filter(_.contains("-")).map(p => {
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
  }

  val defaultArgs=Array("/mnt/Scratch/jesse/bible.alignMe.500000.txt", "/mnt/Scratch/jesse/alignment")
  def main(args: Array[String])  = {
    val a = if (args.size >= 2) args else defaultArgs
    reassembleText(a(0), a(1))
  }
}

import wordAlign._
case class tmxWordAlign(tmxDoc: Iterator[Elem], parallelTextFile: String="/tmp/bible.alignMe.txt") {

  type line = (Seq[String], Seq[String], Seq[String], Seq[String])

  val genesis = "/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/XMLConversie/alignment/en_1890_Darby.Gen--nl_1637_Staten.Gen.aligments.xml"

  def getLang(w: Node) = w.attributes.filter(_.key == "lang").value.text


  def tokenizedText(v: Node) = v.descendant.filter(x => Set("w", "pc").contains(x.label)).map(_.text.replaceAll("\\s+", "")).toSeq // .mkString(" ")


  def fastAlign(): Stream[String] = {
    val command = s"fast_align -i $parallelTextFile -N -d -o -v -I 10".split("\\s+").toSeq
    command lineStream
  }

  def addWordAlignment() = {

    println("Youp!")
    // deze twee hoeven niet iedere keer

    val linkedVerses: Iterator[(Node, Node)] = (tmxDoc.flatMap(x => x \\ "tuv")).map(tu => {
      //println(tu)
      val tuvs = (tu \ "tu")
      if (tuvs.size >= 1)
        tuvs(0) -> tuvs(1)
      else
          <nope/> -> <nope/>
    }).filter({ case (x, y) => x.label != "nope" })


    // write input file for fastAlign

    lazy val alignmentFile = new PrintWriter(parallelTextFile)
    val tokenizedLines: Stream[Line] =
      linkedVerses.map({ case (v1, v2) => {
        val t1: Seq[String] = tokenizedText(v1)
        val t2 = tokenizedText(v2)
        val ids1 = (v1 \\ "w").map(getId)
        val ids2 = (v2 \\ "w").map(getId)
        alignmentFile.println(s"${t1.mkString(" ")} ||| ${t2.mkString(" ")}")
        Line(t1, t2, ids1, ids2)
      }
      }).toStream

    println(s"${tokenizedLines.size}")


    alignmentFile.close()

    // run fastAlign



    val pharaohLines = fastAlign()
    reassemble(tokenizedLines, pharaohLines)
  }
}
